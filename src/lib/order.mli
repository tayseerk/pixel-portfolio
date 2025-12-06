(* Buy or sell side *)
type order_type =
  | Buy
  | Sell
[@@deriving sexp, compare]

(* Alias so other modules can refer to [Order.side] if they want. *)
type side = order_type [@@deriving sexp, compare]

(* Market vs limit order *)
type order_kind =
  | Market
  | Limit of Money.cents
[@@deriving sexp, compare]

(* Unique order identifier *)
type order_id = int [@@deriving sexp, compare, hash]

(* Order lifecycle status *)
type order_status =
  | Filled
  | Cancelled
[@@deriving sexp, compare]

(* Order payload *)
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

(* Build a market order *)
val create_market :
  ticker:Ticker.t ->
  type_of_order:order_type ->
  quantity:int ->
  ?id:order_id ->
  t

(* Build a limit order *)
val create_limit :
  ticker:Ticker.t ->
  type_of_order:order_type ->
  quantity:int ->
  limit_price:Money.cents ->
  ?id:order_id ->
  t


(* Mark an order filled *)
val order_filled : t -> t
(* Mark an order cancelled *)
val order_cancelled : t -> t

