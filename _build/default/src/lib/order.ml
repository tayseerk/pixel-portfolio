[@@@warning "-16"]
open Core

type ticker = string [@@deriving sexp, compare]

type order_type =
  | Buy
  | Sell
[@@deriving sexp, compare]

(* alias matching the .mli so [Order.side] exists. *)
type side = order_type [@@deriving sexp, compare]

type order_kind =
  | Market
  | Limit of Money.cents
[@@deriving sexp, compare]

type order_id = int [@@deriving sexp, compare, hash]

type order_status =
  | Filled
  | Cancelled
[@@deriving sexp, compare]

type t = {
  id : order_id;
  ticker : ticker;
  type_of_order : order_type;
  quantity : int;
  kind : order_kind;
  status : order_status;
}
[@@deriving sexp, fields]

type execution = {
  order : t;
  fill_price : Money.cents;
}
[@@deriving sexp, fields]

let next_id =
  let i = ref 0 in
  fun () ->
    incr i;
    !i

(* Helper now takes a non-optional [id], so no warning 16. *)
let create_common ~id ~ticker ~type_of_order ~quantity ~kind =
  { id; ticker; type_of_order; quantity; kind; status = Filled }

let create_market ~ticker ~type_of_order ~quantity ?id =
  let id = Option.value id ~default:(next_id ()) in
  create_common ~id ~ticker ~type_of_order ~quantity ~kind:Market

let create_limit ~ticker ~type_of_order ~quantity ~limit_price ?id =
  let id = Option.value id ~default:(next_id ()) in
  create_common ~id ~ticker ~type_of_order ~quantity
    ~kind:(Limit limit_price)


let order_filled t = { t with status = Filled }
let order_cancelled t = { t with status = Cancelled }

