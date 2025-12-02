open Core

type ticker = string [@@deriving sexp, compare, hash]

type position = {
  ticker : ticker;
  quantity : int;
  avg_cost : Money.cents;
}
[@@deriving sexp, fields]

type t = {
  cash : Money.cents;
  positions : position String.Map.t;
}
[@@deriving sexp]

let empty = { cash = 0; positions = String.Map.empty }

let of_cash initial_cash = { cash = initial_cash; positions = String.Map.empty }

let with_cash t new_cash = { t with cash = new_cash }

let position_for t ticker = Map.find t.positions ticker

let update_position t ~ticker ~side ~quantity ~fill_price =
  let open Money in
  match side with
  | Order.Buy ->
      let trade_cost = multiply fill_price quantity in
      let new_cash = subtraction t.cash trade_cost in
      let existing = position_for t ticker in
      let new_pos, new_positions =
        match existing with
        | None ->
            let pos = { ticker; quantity; avg_cost = fill_price } in
            (pos, Map.set t.positions ~key:ticker ~data:pos)
        | Some p ->
            let total_qty = p.quantity + quantity in
            let existing_cost = multiply p.avg_cost p.quantity in
            let added_cost = trade_cost in
            let total_cost = addition existing_cost added_cost in
            let new_avg = total_cost / total_qty in
            let pos = { ticker; quantity = total_qty; avg_cost = new_avg } in
            (pos, Map.set t.positions ~key:ticker ~data:pos)
      in
      ignore new_pos;
      { cash = new_cash; positions = new_positions }
  | Order.Sell ->
      (* Cash goes up by price * quantity. *)
      let proceeds = multiply fill_price quantity in
      let new_cash = addition t.cash proceeds in
      let existing = position_for t ticker in
      let new_positions =
        match existing with
        | None ->
            (* For now, ignore sells without an existing position. *)
            t.positions
        | Some p ->
            let remaining_qty = p.quantity - quantity in
            if remaining_qty <= 0 then
              (* Position fully closed or oversold: drop it. *)
              Map.remove t.positions ticker
            else
              let updated = { p with quantity = remaining_qty } in
              Map.set t.positions ~key:ticker ~data:updated
      in
      { cash = new_cash; positions = new_positions }

let all_positions t = Map.data t.positions

let market_value ~prices t =
  Map.fold t.positions ~init:0 ~f:(fun ~key:_ ~data:pos acc ->
      match Map.find prices pos.ticker with
      | None -> acc
      | Some px ->
          let open Money in
          let value = multiply px pos.quantity in
          addition acc value)


let equity ~prices t =
  let open Money in
  let mv = market_value ~prices t in
  addition t.cash mv
