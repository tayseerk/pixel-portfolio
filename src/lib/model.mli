open Core


(** This is the market simulation engine that coordinate stocks.
   This manages stocks, each has its own stochastic process, either
   GBM or OU and an initial price *)

(* Type: stochastic model for a stock. *)
type process =
  | GBM of Gbm.t
  | OU of Ou.t
[@@deriving sexp]

(* Type: ticker, process, and initial price *)
type asset = {
  ticker : Ticker.t;
  process : process;
  initial_price : Money.cents;
}
[@@deriving sexp, fields]

(* Type: map of ticker string to asset *)
type universe = asset String.Map.t [@@deriving sexp]

(* Value: no assets *)
val empty_universe : universe

(* Function: insert/replace an asset by ticker key *)
val add_asset : universe -> asset -> universe

(* Function: lookup asset by ticker *)
val find_asset : universe -> Ticker.t -> asset option

(* Function: map ticker -> initial price (cents) *)
val initial_prices : universe -> Money.cents String.Map.t

(* Function: convert cents to float dollars *)
val price_to_float : Money.cents -> float

(* Function: convert float dollars to cents *)
val float_to_price : float -> Money.cents

(* Function: step one asset using optional noise *)
val evolve_price : asset -> Money.cents -> float option -> Money.cents

(* Function: advance all assets one step given prices/noises *)
val step_universe :
  universe ->
  current_prices:Money.cents String.Map.t ->
  noises:float String.Map.t ->
  Money.cents String.Map.t
