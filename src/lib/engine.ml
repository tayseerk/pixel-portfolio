open Core

type config = {
  universe : Model.universe;
  initial_prices : Money.cents String.Map.t;
  initial_cash : Money.cents;
}
[@@deriving sexp]

type t = {
  config : config;
  time_index : int;
  prices : Money.cents String.Map.t;
  portfolio : Portfolio.t;
  open_orders : Order.t list;
  level : int;
}
[@@deriving sexp]

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

let prices t = t.prices
let portfolio t = t.portfolio
let time_index t = t.time_index

let config t = t.config

let level t = t.level

let with_portfolio t portfolio = { t with portfolio }
let with_prices t prices = { t with prices }

let open_orders t = t.open_orders
let add_open_order t order = { t with open_orders = order :: t.open_orders }
let clear_open_orders t = { t with open_orders = [] }
let cancel_order t order_id =
  let remaining =
    List.filter t.open_orders ~f:(fun o -> o.Order.id <> order_id)
  in
  { t with open_orders = remaining }
(*new one*)
let equity t =
  Portfolio.equity ~prices:t.prices t.portfolio
(*new one*)
let compute_level config ~equity_cents =
  (* Base equity in dollars at level 1 *)
  let base_cash = Money.to_float_dollars config.initial_cash in
  let current = Money.to_float_dollars equity_cents in
  (* No level-ups if you're at or below starting cash *)
  if Float.(current <= base_cash) then 1
  else
    let percent_gain = (current -. base_cash) /. base_cash in
    (* percent_gain is e.g. 0.05 for +5%, 0.45 for +45%, etc. *)
    let rec loop level acc =
      let next_level = level + 1 in
      (* band_index: 1 for levels 1–10, 2 for 11–20, etc. *)
      let band_index = ((next_level - 1) / 10) + 1 in
      let step_gain = 0.05 *. Float.of_int band_index in
      let next_acc = acc +. step_gain in
      if Float.(percent_gain < next_acc) then
        level
      else
        loop next_level next_acc
    in
    loop 1 0.0

(*new one*)
let update_level t =
  let eq = equity t in
  let target = compute_level t.config ~equity_cents:eq in
  let new_level = Int.max t.level target in
  { t with level = new_level }

let should_fill_limit order ~current_price =
  match order.Order.kind with
  | Market -> true
  | Limit limit_price ->
      (match order.Order.type_of_order with
       | Order.Buy -> Money.compare current_price limit_price <= 0
       | Order.Sell -> Money.compare current_price limit_price >= 0)
  | Stop_loss _ -> false

let should_trigger_stop_loss order ~current_price =
  match order.Order.kind with
  | Stop_loss stop_price ->
      (match order.Order.type_of_order with
       | Order.Buy -> Money.compare current_price stop_price >= 0
       | Order.Sell -> Money.compare current_price stop_price <= 0)
  | _ -> false


let apply_execution t execution =
  let open Money in
  let order = execution.Order.order in
  let cost = multiply execution.fill_price order.Order.quantity in
  (* Remove this order from the open order book no matter what. *)
  let remaining_open =
    List.filter t.open_orders ~f:(fun o -> o.Order.id <> order.Order.id)
  in
  match order.Order.type_of_order with
  | Order.Buy ->
      (* Enforce non-negative cash: if we can't afford it, cancel instead. *)
      if Money.compare cost t.portfolio.Portfolio.cash > 0 then
        let _cancelled = Order.order_cancelled order in
        { t with open_orders = remaining_open }
      else
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
      let new_portfolio =
        Portfolio.update_position t.portfolio
          ~ticker:order.Order.ticker
          ~side:order.Order.type_of_order
          ~quantity:order.Order.quantity
          ~fill_price:execution.fill_price
      in
      { t with portfolio = new_portfolio; open_orders = remaining_open }
      |> update_level


let submit_order t ~order =
  match order.Order.kind with
  | Order.Market ->
      (* Immediately fill a market order at the current price if available. *)
      (match Map.find t.prices (Ticker.to_string order.Order.ticker) with
       | None ->
           (* No price for this ticker: just record as open, no execution. *)
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
           let filled_order = Order.order_filled order in
           let exec : Order.execution =
             { order = filled_order; fill_price = current_price }
           in
           let t_after = add_open_order t filled_order in
           let t' = apply_execution t_after exec in
           (t', Some exec)
       | _ ->
           let t' = add_open_order t order in
           (t', None))
  | Order.Stop_loss _ ->
      (* Stop-loss orders are always recorded as open and only checked during ticks. *)
      let t' = add_open_order t order in
      (t', None)


let process_open_orders t =
  let rec loop t pending orders =
    match orders with
    | [] -> { t with open_orders = List.rev pending }
    | order :: rest -> (
        match order.Order.kind with
        | Market ->
            (* Should not generally remain in open_orders, but keep as pending to avoid loss. *)
            loop t (order :: pending) rest
        | Limit _ -> (
            match Map.find t.prices (Ticker.to_string order.Order.ticker) with
            | None ->
                loop t (order :: pending) rest
            | Some current_price ->
                if should_fill_limit order ~current_price then
                  let filled_order = Order.order_filled order in
                  let exec : Order.execution =
                    { order = filled_order; fill_price = current_price }
                  in
                  let t = apply_execution t exec in
                  loop t pending rest
                else
                  loop t (order :: pending) rest)
        | Stop_loss _ -> (
            match Map.find t.prices (Ticker.to_string order.Order.ticker) with
            | None ->
                loop t (order :: pending) rest
            | Some current_price ->
                if should_trigger_stop_loss order ~current_price then
                  let filled_order = Order.order_filled order in
                  let exec : Order.execution =
                    { order = filled_order; fill_price = current_price }
                  in
                  let t = apply_execution t exec in
                  loop t pending rest
                else
                  loop t (order :: pending) rest))
  in
  loop t [] t.open_orders

let tick t ~noise =
  let noise =
    Noise.ensure_noise_map
      ~universe:t.config.universe
      ~existing:noise
  in
  let new_prices =
    Model.step_universe t.config.universe
      ~current_prices:t.prices ~noises:noise
  in
  let updated =
    { t with
      time_index = t.time_index + 1;
      prices = new_prices;
    }
  in
  updated |> process_open_orders |> update_level

let reconcile_open_orders t =
  t |> process_open_orders |> update_level

