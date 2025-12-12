open Core

(* Type: initial settings for a new game *)
type config = {
   universe : Model.universe;
   initial_prices : Money.cents String.Map.t;
   initial_cash : Money.cents;
}
[@@deriving sexp]

(* Type: live engine state (config, prices, portfolio, orders, level). *)
type t [@@deriving sexp]

(* Function: build a new engine from config. *)
val create : config -> t

(* Function: accessor for current price map *)
val prices : t -> Money.cents String.Map.t

(* Function: accessor for portfolio state *)
val portfolio : t -> Portfolio.t

(* Function: accessor for simulation step *)
val time_index : t -> int

(* Function: accessor for engine config *)
val config : t -> config

(* Function: return a copy with new portfolio *)
val with_portfolio : t -> Portfolio.t -> t

(* Function: return a copy with new prices *)
val with_prices : t -> Money.cents String.Map.t -> t

(* Function: accessor for open orders list *)
val open_orders : t -> Order.t list

(* Function: prepend an open order *)
val add_open_order : t -> Order.t -> t

(* Function: remove all open orders *)
val clear_open_orders : t -> t

(* Function: remove an open order by id *)
val cancel_order : t -> Order.order_id -> t

(* Function: record/execute an order, maybe returning an execution *)
val submit_order :
  t ->
  order:Order.t ->
  t * Order.execution option

(* Function: apply a filled order to state *)
val apply_execution : t -> Order.execution -> t

(* Function: advance one step with optional noise *)
val tick :
  t ->
  noise:float String.Map.t ->
  t

(* Function: compute total equity from cash + positions *)
val equity : t -> Money.cents

(* Function: accessor for current player level *)
val level : t -> int

(* Function: fill open orders at current prices without advancing time *)
val reconcile_open_orders : t -> t
