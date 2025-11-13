open Core

type ticker = string [@@deriving sexp, compare, hash]

type config = {
  universe : Model.universe;
  initial_prices : Money.cents String.Map.t;
  initial_cash : Money.cents;
}
[@@deriving sexp]

type t = {
  config : config;
  time_index : int;
  prices : Money.cents String.Map.t;
  portfolio : Portfolio.t;
  open_orders : Order.t list;
}

let create config =
  let prices =
    if Map.is_empty config.initial_prices then
      Model.initial_prices config.universe
    else
      config.initial_prices
  in
  let portfolio = Portfolio.empty ~initial_cash:config.initial_cash in
  { config; time_index = 0; prices; portfolio; open_orders = [] }

let prices t = t.prices
let portfolio t = t.portfolio
let time_index t = t.time_index

let config t = t.config

let with_portfolio t portfolio = { t with portfolio }
let with_prices t prices = { t with prices }

let open_orders t = t.open_orders
let add_open_order t order = { t with open_orders = order :: t.open_orders }
let clear_open_orders t = { t with open_orders = [] }

let submit_order t ~order =
  let t' = add_open_order t order in
  (t', None)

let apply_execution t _execution =
  (* Stub: will finish to portfolio later. *)
  t

let tick t ~noise:_ =
  { t with time_index = t.time_index + 1 }

let equity t =
  Portfolio.equity ~prices:t.prices t.portfolio

let level t =
  (* still just based on time for now. *)
  if t.time_index < 5 then 1
  else if t.time_index < 10 then 2
  else 3

