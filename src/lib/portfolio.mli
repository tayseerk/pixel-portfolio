open Core

(** Portfolio management: track cash, holdings, and equity. *)

(* Type: long or short orientation *)
type direction =
  | Long
  | Short
[@@deriving sexp, compare]

(* Type: position in a ticker. *)
type position = {
  ticker : Ticker.t;
  quantity : int;
  avg_cost : Money.cents;
  direction : direction;
}
[@@deriving sexp, fields]

(* Type: portfolio with cash and open positions by ticker. *)
type t = {
  cash : Money.cents;
  positions : position String.Map.t;
}
[@@deriving sexp]

(* Value: empty portfolio with no positions. *)
val empty : t

(* Function: build a portfolio from starting cash *)
val of_cash : Money.cents -> t

(* Function: copy portfolio with updated cash *)
val with_cash : t -> Money.cents -> t

(* Function: find the open position for a ticker, if any. *)
val position_for : t -> Ticker.t -> position option

(* Function: apply a filled trade to the portfolio. *)
val update_position :
  t ->
  ticker:Ticker.t ->
  side:Order.side ->
  quantity:int ->
  fill_price:Money.cents ->
  t

(* Function: compute total market value of open positions. *)
val market_value : prices:Money.cents String.Map.t -> t -> Money.cents

(* Function: compute total equity (cash + market value). *)
val equity : prices:Money.cents String.Map.t -> t -> Money.cents

(* Function: list all open positions. *)
val all_positions : t -> position list
