open Core

let pretty_money amount = Money.make_it_look_nice amount

(* Internal helper: initial or configured price map for deltas *)
let initial_price_map (engine : Engine.t) =
  let cfg = Engine.config engine in
  if Map.is_empty cfg.initial_prices then
    Model.initial_prices cfg.universe
  else
    cfg.initial_prices

(* Show current prices with deltas vs initial *)
let print_prices engine =
  let current = Engine.prices engine in
  let reference = initial_price_map engine in
  printf "\nCurrent prices (t=%d):\n" (Engine.time_index engine);
  Map.iteri current ~f:(fun ~key:ticker ~data:price ->
      let ref_price =
        Map.find reference ticker
        |> Option.value ~default:price
      in
      let delta = Money.(price $- ref_price) in
      let pct =
        if Money.to_int_cents ref_price = 0 then 0.0
        else
          let d = Money.to_float_dollars delta in
          let r = Money.to_float_dollars ref_price in
          (d /. r) *. 100.0
      in
      printf "  %s: $%s (%s%s, %.2f%%)\n"
        ticker
        (pretty_money price)
        (if Money.to_int_cents delta >= 0 then "+" else "")
        (pretty_money delta)
        pct);
  printf "%!"

(* Show cash, equity, positions, open orders *)
let print_positions engine =
  let portfolio = Engine.portfolio engine in
  printf "\nCash: $%s\n" (pretty_money portfolio.Portfolio.cash);
  printf "Equity (cash + positions): $%s\n"
    (pretty_money (Engine.equity engine));
  printf "Level: %d\n" (Engine.level engine);
  let positions = Portfolio.all_positions portfolio in
  if List.is_empty positions then
    printf "Positions: none\n"
  else (
    printf "Positions:\n";
    List.iter positions ~f:(fun pos ->
        let dir =
          match pos.Portfolio.direction with
          | Portfolio.Long -> "LONG"
          | Portfolio.Short -> "SHORT"
        in
        printf "  - %s: %s %d share(s) @ $%s avg\n"
          (Ticker.to_string pos.Portfolio.ticker)
          dir
          pos.Portfolio.quantity
          (pretty_money pos.Portfolio.avg_cost)));
  let open_orders = Engine.open_orders engine in
  if List.is_empty open_orders then
    printf "Open orders: none\n"
  else (
    printf "Open orders:\n";
    List.iter open_orders ~f:(fun order ->
        let side =
          match order.Order.type_of_order with
          | Buy -> "BUY"
          | Sell -> "SELL"
        in
        let kind =
          match order.Order.kind with
          | Market -> "market"
          | Limit px -> "limit $" ^ pretty_money px
          | Stop_loss px -> "stop-loss $" ^ pretty_money px
        in
        printf "  - #%d %s %d %s (%s)\n"
          order.Order.id side order.Order.quantity
          (Ticker.to_string order.Order.ticker) kind))
