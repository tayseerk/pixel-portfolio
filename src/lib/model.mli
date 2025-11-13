(** This is the market simulation engine that coordinate stocks.
    This manages stocks, each has its own stochastic process, either
    GBM or OU and an initial price *)

open Core

type ticker = string [@@deriving sexp, compare, hash] (* stock ticker symbol again as placeholder *)

(* type of stchastic model for a stock *)
type process =
  | GBM of Gbm.t (* Geometric Brownian Motion process *)
  | OU of Ou.t (* Ornstein-Uhlenbeck process *)
[@@deriving sexp]

(* description of a single asses in a simulated game or universe*)
type asset = {
  ticker : ticker;
  process : process;
  initial_price : Money.cents;
}
[@@deriving sexp, fields]

(* full set of assets *)
type universe = asset String.Map.t [@@deriving sexp]

(* universe or game with no assets *)
val empty_universe : universe

(* returning new universe with assets inserted *)
val add_asset : universe -> asset -> universe

(* finding asset in universe *)
val find_asset : universe -> ticker -> asset option

(* returning map from ticker for every asset with the prices in cent *)
val initial_prices : universe -> Money.cents String.Map.t

(* Advancing all assets in the game by one time step *)
val step_universe :
  universe ->
  current_prices:Money.cents String.Map.t ->
  noises:float String.Map.t ->
  Money.cents String.Map.t
