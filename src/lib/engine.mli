open Core

(** config to start a new game instance *)
type config = {
   universe : Model.universe;
   initial_prices : Money.cents String.Map.t;
   initial_cash : Money.cents;
}
[@@deriving sexp]

type t

(**create new engine state from the config above *)
val create : config -> t

(** curr prices for all the tickers (in cents) *)
val prices : t -> Money.cents String.Map.t

(** curr portfolio (has the amount of cash and the positions) *)
val portfolio : t -> Portfolio.t

(** curr index of the step *)
val time_index : t -> int

val config : t -> config

(** return a new engine with the portfolio *)
val with_portfolio : t -> Portfolio.t -> t

(** return a new engine with the price map *)
val with_prices : t -> Money.cents String.Map.t -> t

(** holds all the open orders in the curr engine *)
val open_orders : t -> Order.t list

(** add an open order to the state of the curr engine *)
val add_open_order : t -> Order.t -> t

(** remove all open orders in the state of the curr engine *)
val clear_open_orders : t -> t

(** when we implement the code, this will submit a new order to the curr engine *)
(** for now though it simply records the order *)
val submit_order :
  t ->
  order:Order.t ->
  t * Order.execution option

(** apply a filled order to the curr engine *)
val apply_execution : t -> Order.execution -> t

(** move the game/sim by one tick or step*)
(** noise will be implemented later, optional at the moment since it drives price dynamics *)
val tick :
  t ->
  noise:float String.Map.t ->
  t

(** cash + positions of the player in cents *)
val equity : t -> Money.cents

(** easy, medium, hard where easy gives you the most money, medium 2nd most, and hard gives you the least amount*)
val level : t -> int
