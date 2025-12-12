open Core

(* Type: stochastic process (GBM or OU) for an asset *)
type process =
  | GBM of Gbm.t
  | OU of Ou.t
[@@deriving sexp]

(* Type: asset (ticker + process + initial price) *)
type asset = {
  ticker : Ticker.t;
  process : process;
  initial_price : Money.cents;
}
[@@deriving sexp, fields]

(* Type: map of ticker string to asset *)
type universe = asset String.Map.t [@@deriving sexp]

(* Value: no assets *)
let empty_universe = String.Map.empty

(* Function: insert/replace an asset by ticker key *)
let add_asset universe asset =
  Map.set universe
    ~key:(Ticker.to_string asset.ticker)
    ~data:asset

(* Function: lookup asset by ticker *)
let find_asset universe ticker =
  Map.find universe (Ticker.to_string ticker)

(* Function: map of ticker -> initial price *)
let initial_prices universe =
  String.Map.map universe ~f:(fun a -> a.initial_price)

(* Function: convert cents to float dollars *)
let price_to_float (c : Money.cents) : float =
  Money.to_float_dollars c

(* Function: convert float dollars to cents *)
let float_to_price (d : float) : Money.cents =
  Money.of_float_dollars d

(* Function: step one asset with optional noise *)
let evolve_price asset current_price noise_opt =
  let step =
    match asset.process with
    | GBM gbm -> Gbm.step gbm
    | OU ou -> Ou.step ou
  in
  let noise = Option.value noise_opt ~default:0.0 in
  step ~price:(price_to_float current_price) ~noise
  |> float_to_price

(* Function: step all assets given current prices and noises *)
let step_universe universe ~current_prices ~noises =
  Map.mapi universe ~f:(fun ~key:ticker_str ~data:asset ->
      let current_price =
        match Map.find current_prices ticker_str with
        | Some px -> px
        | None -> asset.initial_price
      in
      let noise = Map.find noises ticker_str in
      evolve_price asset current_price noise)

