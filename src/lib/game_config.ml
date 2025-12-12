open Core

(* Type: difficulty levels *)
type difficulty =
  | Easy
  | Medium
  | Hard
[@@deriving sexp, compare]

(* Value: default save filename *)
let default_state_file = "game.sexp"

(* Function: initial cash per difficulty *)
let starting_cash = function
  | Easy -> Money.of_float_dollars 100_000.0
  | Medium -> Money.of_float_dollars 25_000.0
  | Hard -> Money.of_float_dollars 5_000.0

(* Function: convert difficulty to string *)
let to_string = function
  | Easy -> "easy"
  | Medium -> "medium"
  | Hard -> "hard"

(* Function: parse difficulty string *)
let of_string s =
  match String.lowercase (String.strip s) with
  | "easy" -> Some Easy
  | "medium" -> Some Medium
  | "hard" -> Some Hard
  | _ -> None

(* Value: all difficulty values *)
let all = [ Easy; Medium; Hard ]

(* Function: build a GBM-modeled asset *)
let mk_gbm_asset ticker_sym ~price ~mu ~sigma =
  let process = Model.GBM (Gbm.create ~mu ~sigma ~dt:1.0) in
  let ticker = Ticker.of_string ticker_sym in
  { Model.ticker; process; initial_price = Money.of_float_dollars price }

(* Function: build an OU-modeled asset *)
let mk_ou_asset ticker_sym ~price ~kappa ~theta ~sigma =
  let process = Model.OU (Ou.create ~kappa ~theta ~sigma ~dt:1.0) in
  let ticker = Ticker.of_string ticker_sym in
  { Model.ticker; process; initial_price = Money.of_float_dollars price }

(* Value: base universe of GBM/OU assets *)
let base_universe =
    let trending =
    [
      mk_gbm_asset "AAPL" ~price:195.0 ~mu:0.05 ~sigma:0.18;
      mk_gbm_asset "AMZN" ~price:140.0 ~mu:0.05 ~sigma:0.20;
      mk_gbm_asset "GOOG" ~price:125.0 ~mu:0.04 ~sigma:0.18;
      mk_gbm_asset "NVDA" ~price:450.0 ~mu:0.07 ~sigma:0.30;
      mk_gbm_asset "PLTR" ~price:18.5 ~mu:0.08 ~sigma:0.28;
    ]

    and penny =
    [
      mk_ou_asset "DKSC" ~price:1.20 ~kappa:1.0 ~theta:1.10 ~sigma:0.70;
      mk_ou_asset "RNWF" ~price:0.80 ~kappa:0.9 ~theta:0.75 ~sigma:0.80;
      mk_ou_asset "BCEKF" ~price:1.90 ~kappa:0.8 ~theta:1.85 ~sigma:0.60;
      mk_ou_asset "SVRSF" ~price:1.05 ~kappa:0.9 ~theta:1.0  ~sigma:0.70;
      mk_ou_asset "T1E"  ~price:2.50 ~kappa:0.7 ~theta:2.4  ~sigma:0.60;
    ]
  in
  List.append trending penny
  |> List.fold ~init:Model.empty_universe ~f:Model.add_asset
