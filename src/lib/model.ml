open Core

type process =
  | GBM of Gbm.t
  | OU of Ou.t
[@@deriving sexp]

type asset = {
  ticker : Ticker.t;
  process : process;
  initial_price : Money.cents;
}
[@@deriving sexp, fields]

type universe = asset String.Map.t [@@deriving sexp]

let empty_universe = String.Map.empty

let add_asset universe asset =
  Map.set universe
    ~key:(Ticker.to_string asset.ticker)
    ~data:asset

let find_asset universe ticker =
  Map.find universe (Ticker.to_string ticker)

let initial_prices universe =
  String.Map.map universe ~f:(fun a -> a.initial_price)

let price_to_float (c : Money.cents) : float =
  Money.to_float_dollars c

let float_to_price (d : float) : Money.cents =
  Money.of_float_dollars d

let evolve_price asset current_price noise_opt =
  let step =
    match asset.process with
    | GBM gbm -> Gbm.step gbm
    | OU ou -> Ou.step ou
  in
  let noise = Option.value noise_opt ~default:0.0 in
  step ~price:(price_to_float current_price) ~noise
  |> float_to_price

let step_universe universe ~current_prices ~noises =
  Map.mapi universe ~f:(fun ~key:ticker_str ~data:asset ->
      let current_price =
        match Map.find current_prices ticker_str with
        | Some px -> px
        | None -> asset.initial_price
      in
      let noise = Map.find noises ticker_str in
      evolve_price asset current_price noise)

