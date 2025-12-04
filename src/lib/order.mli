type order_type =
  | Buy
  | Sell
[@@deriving sexp, compare]

(* Alias so other modules can refer to [Order.side] if they want. *)
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
  ticker : Ticker.t;
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

val create_market :
  ticker:Ticker.t ->
  type_of_order:order_type ->
  quantity:int ->
  ?id:order_id ->
  t

val create_limit :
  ticker:Ticker.t ->
  type_of_order:order_type ->
  quantity:int ->
  limit_price:Money.cents ->
  ?id:order_id ->
  t


val order_filled : t -> t
val order_cancelled : t -> t

