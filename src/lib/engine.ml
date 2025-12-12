open Core

(* Type: initial setup inputs (universe, initial prices/cash) *)
type config = {
  universe : Model.universe;
  initial_prices : Money.cents String.Map.t;
  initial_cash : Money.cents;
}
[@@deriving sexp]

(* Type: live engine state (config + prices, portfolio, orders, level) *)
type t = {
  config : config;
  time_index : int;
  prices : Money.cents String.Map.t;
  portfolio : Portfolio.t;
  open_orders : Order.t list;
  level : int;
}
[@@deriving sexp]

(* Function: build a fresh engine from a config *)
let create config =
  let prices =
    if Map.is_empty config.initial_prices then
      Model.initial_prices config.universe
    else
      config.initial_prices
  in
  let portfolio = Portfolio.of_cash config.initial_cash in
  { config;
    time_index = 0;
    prices;
    portfolio;
    open_orders = [];
    level = 1 }

(* Function: accessor for current price map *)
let prices t = t.prices

(* Function: accessor for portfolio state *)
let portfolio t = t.portfolio

(* Function: accessor for simulation step *)
let time_index t = t.time_index

(* Function: accessor for engine config *)
let config t = t.config

(* Function: accessor for current player level *)
let level t = t.level

(* Function: return a copy with new portfolio *)
let with_portfolio t portfolio = { t with portfolio }

(* Function: return a copy with new prices *)
let with_prices t prices = { t with prices }

(* Function: accessor for open orders list *)
let open_orders t = t.open_orders

(* Function: prepend an open order *)
let add_open_order t order = { t with open_orders = order :: t.open_orders }

(* Function: remove all open orders *)
let clear_open_orders t = { t with open_orders = [] }

(* Function: remove an open order by id *)
let cancel_order t order_id =
  let remaining =
    List.filter t.open_orders ~f:(fun o -> o.Order.id <> order_id)
  in
  { t with open_orders = remaining }

(* Function: compute total equity from cash + positions *)
let equity t =
  Portfolio.equity ~prices:t.prices t.portfolio

(* Function: compute target level from equity gains *)
let compute_level config ~equity_cents =
  (* Base equity in dollars at level 1 *)
  let base_cash = Money.to_float_dollars config.initial_cash in
  let current = Money.to_float_dollars equity_cents in
  (* No level-ups if you're at or below starting cash *)
  if Float.(current <= base_cash) then 1
  else
    let percent_gain = (current -. base_cash) /. base_cash in
    (* example: 0.05 for +5% *)
    let rec loop level acc =
      let next_level = level + 1 in
      (* 1 for levels 1–10, 2 for 11–20, etc. *)
      let band_index = ((next_level - 1) / 10) + 1 in
      let step_gain = 0.05 *. Float.of_int band_index in
      let next_acc = acc +. step_gain in
      if Float.(percent_gain < next_acc) then
        level
      else
        loop next_level next_acc
    in
    loop 1 0.0

(* Function: update engine level based on equity *)
let update_level t =
  let eq = equity t in
  let target = compute_level t.config ~equity_cents:eq in
  let new_level = Int.max t.level target in
  { t with level = new_level }

(* Function: check if limit order crosses current price *)
let should_fill_limit order ~current_price =
  match order.Order.kind with
  | Market -> true
  | Limit limit_price ->
      (match order.Order.type_of_order with
       | Order.Buy -> Money.compare current_price limit_price <= 0
       | Order.Sell -> Money.compare current_price limit_price >= 0)
  | Stop_loss _ -> false

(* Function: check stop-loss trigger *)
let should_trigger_stop_loss order ~current_price =
  match order.Order.kind with
  | Stop_loss stop_price ->
      (match order.Order.type_of_order with
       | Order.Buy -> Money.compare current_price stop_price >= 0
       | Order.Sell -> Money.compare current_price stop_price <= 0)
  | _ -> false


(* Function: apply a filled order to engine state *)
let apply_execution t execution =
  let open Money in
  let order = execution.Order.order in
  let cost = multiply execution.fill_price order.Order.quantity in
  (* remove this order from the open order book no matter what *)
  let remaining_open =
    List.filter t.open_orders ~f:(fun o -> o.Order.id <> order.Order.id)
  in
  match order.Order.type_of_order with
  | Order.Buy ->
      (* enforce non-negative cash: if we can't afford it, cancel instead *)
      if Money.compare cost t.portfolio.Portfolio.cash > 0 then
        let _cancelled = Order.order_cancelled order in
        { t with open_orders = remaining_open }
      else
        (* update long/short positions and cash, then refresh level *)
        let new_portfolio =
          Portfolio.update_position t.portfolio
            ~ticker:order.Order.ticker
            ~side:order.Order.type_of_order
            ~quantity:order.Order.quantity
            ~fill_price:execution.fill_price
        in
        { t with portfolio = new_portfolio; open_orders = remaining_open }
        |> update_level
  | Order.Sell ->
      (* update positions for sell (can close or flip to short), then refresh level *)
      let new_portfolio =
        Portfolio.update_position t.portfolio
          ~ticker:order.Order.ticker
          ~side:order.Order.type_of_order
          ~quantity:order.Order.quantity
          ~fill_price:execution.fill_price
      in
      { t with portfolio = new_portfolio; open_orders = remaining_open }
      |> update_level


(* Function: add/execute an order and return execution if any *)
let submit_order t ~order =
  match order.Order.kind with
  | Order.Market ->
      (* immediately fill a market order at the current price if available *)
      (match Map.find t.prices (Ticker.to_string order.Order.ticker) with
       | None ->
          (* no price for this ticker: record as open, no execution *)
           let t' = add_open_order t order in
           (t', None)
       | Some px ->
           let filled_order = Order.order_filled order in
           let exec : Order.execution = { order = filled_order; fill_price = px } in
           let t_after = add_open_order t filled_order in
           let t' = apply_execution t_after exec in
           (t', Some exec))
  | Order.Limit _ ->
      (match Map.find t.prices (Ticker.to_string order.Order.ticker) with
       | Some current_price when should_fill_limit order ~current_price ->
           (* limit can fill immediately at current price *)
           let filled_order = Order.order_filled order in
           let exec : Order.execution =
             { order = filled_order; fill_price = current_price }
           in
           let t_after = add_open_order t filled_order in
           let t' = apply_execution t_after exec in
           (t', Some exec)
       | _ ->
           (* otherwise, keep it open until later ticks *)
           let t' = add_open_order t order in
           (t', None))
  | Order.Stop_loss _ ->
      (* stop-loss orders are always recorded as open and only checked during ticks *)
      let t' = add_open_order t order in
      (t', None)


(* Function: walk open orders and fill those that conditions are met at current price *)
let process_open_orders t =
  let rec loop t pending orders =
    match orders with
    | [] -> { t with open_orders = List.rev pending } (* nothing left, keep pending list *)
    | order :: rest -> (
        match order.Order.kind with
        | Market ->
            (* market orders should rarely be here, keep pending to avoid dropping *)
            loop t (order :: pending) rest
        | Limit _ -> (
            match Map.find t.prices (Ticker.to_string order.Order.ticker) with
            | None ->
                (* no price yet, keep waiting *)
                loop t (order :: pending) rest
            | Some current_price ->
                if should_fill_limit order ~current_price then
                  (* fill limit, apply execution, do not re-queue *)
                  let filled_order = Order.order_filled order in
                  let exec : Order.execution =
                    { order = filled_order; fill_price = current_price }
                  in
                  let t = apply_execution t exec in
                  loop t pending rest
                else
                  (* price not crossed, keep waiting *)
                  loop t (order :: pending) rest)
        | Stop_loss _ -> (
            match Map.find t.prices (Ticker.to_string order.Order.ticker) with
            | None ->
                (* no price yet, keep waiting *)
                loop t (order :: pending) rest
            | Some current_price ->
                if should_trigger_stop_loss order ~current_price then
                  (* trigger stop-loss, apply execution *)
                  let filled_order = Order.order_filled order in
                  let exec : Order.execution =
                    { order = filled_order; fill_price = current_price }
                  in
                  let t = apply_execution t exec in
                  loop t pending rest
                else
                  (* not triggered, keep waiting *)
                  loop t (order :: pending) rest))
  in
  loop t [] t.open_orders

(* Function: advance one step with noise, prices, orders, and level *)
let tick t ~noise =
  (* normalize noise map to cover all tickers in the universe *)
  let noise =
    Noise.ensure_noise_map
      ~universe:t.config.universe
      ~existing:noise
  in
  (* advance price paths one step using the noise *)
  let new_prices =
    Model.step_universe t.config.universe
      ~current_prices:t.prices ~noises:noise
  in
  (* increment time and store the stepped prices *)
  let updated =
    { t with
      time_index = t.time_index + 1;
      prices = new_prices;
    }
  in
  (* after prices move, fill any eligible open orders and refresh level *)
  updated |> process_open_orders |> update_level

(* Function: fill any open orders at current prices *)
let reconcile_open_orders t =
  t |> process_open_orders |> update_level

