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
}

let random_state = Random.State.make [| 0x5eed_cafe |]

let create config =
  let prices =
    if Map.is_empty config.initial_prices then
      Model.initial_prices config.universe
    else
      config.initial_prices
  in
  let portfolio = Portfolio.of_cash config.initial_cash in
  { config; time_index = 0; prices; portfolio; open_orders = [] }

let prices t = t.prices
let portfolio t = t.portfolio
let time_index t = t.time_index

let config t = t.config

let with_portfolio t portfolio = { t with portfolio }
let with_prices t prices = { t with prices }

let open_orders t = t.open_orders
let add_open_order t order = { t with open_orders = order :: t.open_orders }
let clear_open_orders t = { t with open_orders = [] }

let apply_execution t execution =
  let order = execution.Order.order in
  (* Update portfolio cash and positions. *)
  let new_portfolio =
    Portfolio.update_position t.portfolio
      ~ticker:order.Order.ticker
      ~side:order.Order.type_of_order
      ~quantity:order.Order.quantity
      ~fill_price:execution.fill_price
  in
  (* Drop this order from the open_orders list. *)
  let new_open_orders =
    List.filter t.open_orders ~f:(fun o -> o.Order.id <> order.Order.id)
  in
  { t with portfolio = new_portfolio; open_orders = new_open_orders }

let submit_order t ~order =
  match order.Order.kind with
  | Order.Market ->
      (* Immediately fill a market order at the current price if available. *)
      (match Map.find t.prices order.Order.ticker with
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
      (* For now, we don't simulate matching logic: keep as open, no execution. *)
      let t' = add_open_order t order in
      (t', None)

let sample_noise () =
  Random.State.float random_state 2.0 -. 1.0

let ensure_noise_map t noise =
  Map.fold t.config.universe ~init:noise ~f:(fun ~key ~data:_ acc ->
      if Map.mem acc key then acc else Map.set acc ~key ~data:(sample_noise ()))

let tick t ~noise =
  let noise = ensure_noise_map t noise in
  let new_prices =
    Model.step_universe t.config.universe
      ~current_prices:t.prices ~noises:noise
  in
  { t with
    time_index = t.time_index + 1;
    prices = new_prices;
  }

let equity t =
  Portfolio.equity ~prices:t.prices t.portfolio

let level t =
  (* still just based on time for now. *)
  if t.time_index < 5 then 1
  else if t.time_index < 10 then 2
  else 3

