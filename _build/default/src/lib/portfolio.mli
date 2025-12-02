open Core

(** The portfolio management for pixel-portfolio
   This module will track the player's cash, holdings, equity,
   and the level of the player*)
type ticker = string [@@deriving sexp, compare, hash]

(* Position in a ticker *)
type position = {
  ticker : ticker;
  quantity : int;
  avg_cost : Money.cents;
}
[@@deriving sexp, fields]

(* Portfolio with cash and open positions by ticker *)
type t = {
  cash : Money.cents;
  positions : position String.Map.t;
}
[@@deriving sexp]

(* create a new portfolio with the given cash and no positions open *)
val empty : t

val of_cash : Money.cents -> t

val with_cash : t -> Money.cents -> t

(* returning the open position for a ticker in the portfolio, none if no position exists*)
val position_for : t -> ticker -> position option

(* Returning new portfolio that is obtained by applying a filled trde to the portfolio *)
val update_position :
  t ->
  ticker:ticker ->
  side:Order.side ->
  quantity:int ->
  fill_price:Money.cents ->
  t

(** compute the total market value of all the open positions in a portfolio
   using the map*)
val market_value : prices:Money.cents String.Map.t -> t -> Money.cents

(* Returning the total equity of a portfolio using given prices *)
val equity : prices:Money.cents String.Map.t -> t -> Money.cents

(* Returnign all open positions in a list *)
val all_positions : t -> position list
