open Core

type direction =
  | Long
  | Short
[@@deriving sexp, compare]

type position = {
  ticker : Ticker.t;
  quantity : int;
  avg_cost : Money.cents;
  direction : direction;
}
[@@deriving sexp, fields]

type t = {
  cash : Money.cents;
  positions : position String.Map.t;
}
[@@deriving sexp]

let empty = { cash = Money.of_int_cents 0; positions = String.Map.empty }

let of_cash initial_cash = { cash = initial_cash; positions = String.Map.empty }

let with_cash t new_cash = { t with cash = new_cash }

let position_for t ticker =
  Map.find t.positions (Ticker.to_string ticker)

let update_position t ~ticker ~side ~quantity ~fill_price =
  let open Money in
  let existing = position_for t ticker in
  match (side, existing) with
  (* --- No existing position --- *)
  | Order.Buy, None ->
      (* Open new long. *)
      let trade_cost = fill_price $* quantity in
      let new_cash = t.cash $- trade_cost in
      let pos =
        { ticker; quantity; avg_cost = fill_price; direction = Long }
      in
      { cash = new_cash;
        positions = Map.set t.positions ~key:(Ticker.to_string ticker) ~data:pos }
  | Order.Sell, None ->
      (* Open new short. *)
      let proceeds = fill_price $* quantity in
      let new_cash = t.cash $+ proceeds in
      let pos =
        { ticker; quantity; avg_cost = fill_price; direction = Short }
      in
      { cash = new_cash;
        positions = Map.set t.positions ~key:(Ticker.to_string ticker) ~data:pos }

  (* --- Existing position; incoming BUY --- *)
  | Order.Buy, Some p -> (
      match p.direction with
      | Long ->
          (* Increase existing long. *)
          let trade_cost = fill_price $* quantity in
          let new_cash = t.cash $- trade_cost in
          let total_qty = p.quantity + quantity in
          let existing_cost = p.avg_cost $* p.quantity in
          let added_cost = trade_cost in
          let total_cost = existing_cost $+ added_cost in
          let new_avg = Money.of_int_cents (Money.to_int_cents total_cost / total_qty) in
          let pos =
            { p with quantity = total_qty; avg_cost = new_avg }
          in
          { cash = new_cash;
            positions = Map.set t.positions ~key:(Ticker.to_string ticker) ~data:pos }
      | Short ->
          (* Buy to cover an existing short, possibly crossing to long. *)
          let trade_cost = fill_price $* quantity in
          let new_cash = t.cash $- trade_cost in
          if quantity < p.quantity then
            (* Partial cover: remain short. *)
            let remaining_qty = p.quantity - quantity in
            let pos = { p with quantity = remaining_qty } in
            { cash = new_cash;
              positions = Map.set t.positions ~key:(Ticker.to_string ticker) ~data:pos }
          else if quantity = p.quantity then
            (* Fully flat. *)
            { cash = new_cash; positions = Map.remove t.positions (Ticker.to_string ticker) }
          else
            (* Cross from short to long in one trade. *)
            let remaining_long = quantity - p.quantity in
            let pos =
              { ticker;
                quantity = remaining_long;
                avg_cost = fill_price;
                direction = Long }
            in
            { cash = new_cash;
              positions = Map.set t.positions ~key:(Ticker.to_string ticker) ~data:pos })

  (* --- Existing position; incoming SELL --- *)
  | Order.Sell, Some p -> (
      match p.direction with
      | Long ->
          (* Sell out of a long, maybe cross to short. *)
          let proceeds = fill_price $* quantity in
          let new_cash = t.cash $+ proceeds in
          if quantity < p.quantity then
            let remaining_qty = p.quantity - quantity in
            let pos = { p with quantity = remaining_qty } in
            { cash = new_cash;
              positions = Map.set t.positions ~key:(Ticker.to_string ticker) ~data:pos }
          else if quantity = p.quantity then
            (* Close the long. *)
            { cash = new_cash; positions = Map.remove t.positions (Ticker.to_string ticker) }
          else
            (* Sell more than long: end up short. *)
            let extra_short = quantity - p.quantity in
            let pos =
              { ticker;
                quantity = extra_short;
                avg_cost = fill_price;
                direction = Short }
            in
            { cash = new_cash;
              positions = Map.set t.positions ~key:(Ticker.to_string ticker) ~data:pos }
      | Short ->
          (* Increase the size of an existing short. *)
          let proceeds = fill_price $* quantity in
          let new_cash = t.cash $+ proceeds in
          let total_qty = p.quantity + quantity in
          let existing_proceeds = p.avg_cost $* p.quantity in
          let added_proceeds = fill_price $* quantity in
          let total_proceeds = existing_proceeds $+ added_proceeds in
          let new_avg = Money.of_int_cents (Money.to_int_cents total_proceeds / total_qty) in
          let pos =
            { p with quantity = total_qty; avg_cost = new_avg }
          in
          { cash = new_cash;
            positions = Map.set t.positions ~key:(Ticker.to_string ticker) ~data:pos })

let all_positions t = Map.data t.positions

let market_value ~prices t =
  Map.fold t.positions ~init:(Money.of_int_cents 0)
    ~f:(fun ~key:_ ~data:pos acc ->
      match Map.find prices (Ticker.to_string pos.ticker) with
      | None -> acc
      | Some px ->
          let open Money in
          let unsigned = px $* pos.quantity in
          let signed =
            match pos.direction with
            | Long -> unsigned
            | Short -> unsigned $* (-1)
          in
          acc $+ signed)


let equity ~prices t =
  let open Money in
  let mv = market_value ~prices t in
  t.cash $+ mv
