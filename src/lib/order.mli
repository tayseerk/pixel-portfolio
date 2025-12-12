(* Type: buy or sell side *)
type order_type =
  | Buy
  | Sell
[@@deriving sexp, compare]

(* Alias: side — same as order_type*)
type side = order_type [@@deriving sexp, compare]

(* Type: market, limit, or stop-loss *)
type order_kind =
  | Market
  | Limit of Money.cents
  | Stop_loss of Money.cents
[@@deriving sexp, compare]

(* Type: unique identifier *)
type order_id = int [@@deriving sexp, compare, hash]

(* Type: lifecycle state *)
type order_status =
  | Open
  | Filled
  | Cancelled
[@@deriving sexp, compare]

(* Type: order payload. *)
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

(* Function: build a market order (auto id if omitted) *)
val create_market :
  ticker:Ticker.t ->
  type_of_order:order_type ->
  quantity:int ->
  ?id:order_id ->
  t

(* Function: build a limit order (auto id if omitted) *)
val create_limit :
  ticker:Ticker.t ->
  type_of_order:order_type ->
  quantity:int ->
  limit_price:Money.cents ->
  ?id:order_id ->
  t

(* Function: build a stop-loss order (auto id if omitted) *)
val create_stop_loss :
  ticker:Ticker.t ->
  type_of_order:order_type ->
  quantity:int ->
  stop_price:Money.cents ->
  ?id:order_id ->
  t

(* Function: mark an order filled *)
val order_filled : t -> t

(* Function: mark an order cancelled *)
val order_cancelled : t -> t

