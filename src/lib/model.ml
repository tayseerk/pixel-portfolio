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

let step_universe _universe ~current_prices ~noises:_ =
  (* Stub: prices do not move yet. *)
  current_prices
