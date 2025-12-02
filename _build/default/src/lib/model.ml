open Core

type ticker = string [@@deriving sexp, compare, hash]

type process =
  | GBM of Gbm.t
  | OU of Ou.t
[@@deriving sexp]

type asset = {
  ticker : ticker;
  process : process;
  initial_price : Money.cents;
}
[@@deriving sexp, fields]

type universe = asset String.Map.t [@@deriving sexp]

let empty_universe = String.Map.empty

let add_asset universe asset =
  Map.set universe ~key:asset.ticker ~data:asset

let find_asset universe ticker = Map.find universe ticker

let initial_prices universe =
  Map.map universe ~f:(fun a -> a.initial_price)

let price_to_float cents =
  Money.cents_to_float_dollars cents

let float_to_price dollars =
  Money.float_dollars_to_cents dollars

let evolve_price asset current_price noise =
  let price_float = price_to_float current_price in
  let noise = Option.value noise ~default:0.0 in
  match asset.process with
  | GBM gbm ->
      let next_price = Gbm.step gbm ~price:price_float ~noise in
      float_to_price next_price
  | OU ou ->
      let next_state = Ou.step ou ~state:price_float ~noise in
      float_to_price next_state

let step_universe universe ~current_prices ~noises =
  Map.mapi universe ~f:(fun ~key:ticker ~data:asset ->
      let current_price =
        Map.find current_prices ticker
        |> Option.value ~default:asset.initial_price
      in
      let noise = Map.find noises ticker in
      evolve_price asset current_price noise)
