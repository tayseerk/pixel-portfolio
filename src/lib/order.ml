[@@@warning "-16"]
open Core

(* Type: buy or sell side(direction of the order)*)
type order_type =
  | Buy
  | Sell
[@@deriving sexp, compare]

(* Alias: side — matches .mli so [Order.side] exists. *)
type side = order_type [@@deriving sexp, compare]

(* Type: market, limit, or stop-loss *)
type order_kind =
  | Market
  | Limit of Money.cents
  | Stop_loss of Money.cents
[@@deriving sexp, compare]

(* Type: unique identifier *)
type order_id = int [@@deriving sexp, compare, hash]

(* Type: open, filled, or cancelled *)
type order_status =
  | Open 
  | Filled
  | Cancelled
[@@deriving sexp, compare]

(* Type: order record *)
type t = {
  id : order_id;
  ticker : Ticker.t;
  type_of_order : order_type;
  quantity : int;
  kind : order_kind;
  status : order_status;
}
[@@deriving sexp, fields]

(* Type: filled order with price *)
type execution = {
  order : t;
  fill_price : Money.cents;
}
[@@deriving sexp, fields]

(* Function: generate sequential ids *)
let next_id =
  let i = ref 0 in
  fun () ->
    incr i;
    !i

(* Function: build an order record *)
let create_common ~id ~ticker ~type_of_order ~quantity ~kind =
  { id; ticker; type_of_order; quantity; kind; status = Open }

(* Function: make a market order (auto id) *)
let create_market ~ticker ~type_of_order ~quantity ?id =
  let id = Option.value id ~default:(next_id ()) in
  create_common ~id ~ticker ~type_of_order ~quantity ~kind:Market

(* Function: make a limit order (auto id) *)
let create_limit ~ticker ~type_of_order ~quantity ~limit_price ?id =
  let id = Option.value id ~default:(next_id ()) in
  create_common ~id ~ticker ~type_of_order ~quantity
    ~kind:(Limit limit_price)

(* Function: make a stop-loss order (auto id) *)
let create_stop_loss ~ticker ~type_of_order ~quantity ~stop_price ?id =
  let id = Option.value id ~default:(next_id ()) in
  create_common ~id ~ticker ~type_of_order ~quantity
    ~kind:(Stop_loss stop_price)

(* Function: mark order as filled *)
let order_filled t = { t with status = Filled }

(* Function: mark order as cancelled *)
let order_cancelled t = { t with status = Cancelled }

