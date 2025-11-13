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

let empty ~initial_cash = { cash = initial_cash; positions = String.Map.empty }

let position_for t ticker = Map.find t.positions ticker

let update_position t ~ticker:_ ~side:_ ~quantity:_ ~fill_price:_ =
  (* Stub: real portfolio logic will be added later. *)
  t

let all_positions t = Map.data t.positions

let market_value ~prices:_ _t =
  (* Stub: currently positions are valued at 0. *)
  0


let equity ~prices:_ t =
  (* For now, equity is just cash. *)
  t.cash
