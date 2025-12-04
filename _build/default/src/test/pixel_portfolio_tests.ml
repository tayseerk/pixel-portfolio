open Core
open OUnit2
open Pixel_portfolio_lib

let approx_equal ?(tol = 1e-6) a b =
  Float.(abs (a -. b) < tol)

let gbm_step_test _ctx =
  let params = Gbm.create ~mu:0.1 ~sigma:0.2 ~dt:1.0 in
  let price = 100.0 in
  let expected =
    let drift = (0.1 -. 0.5 *. Float.square 0.2) in
    price *. Float.exp drift
  in
  let actual = Gbm.step params ~price ~noise:0.0 in
  assert_bool "GBM step drift matches expectation"
    (approx_equal actual expected)

let model_step_test _ctx =
  let asset : Model.asset =
    {
      ticker = "PLTR";
      process = Model.GBM (Gbm.create ~mu:0.0 ~sigma:0.2 ~dt:1.0);
      initial_price = Money.float_dollars_to_cents 10.0;
    }
  in
  let universe = Model.add_asset Model.empty_universe asset in
  let prices = Model.initial_prices universe in
  let noises =
    ["PLTR", 0.5] |> String.Map.of_alist_exn
  in
  let new_prices =
    Model.step_universe universe ~current_prices:prices ~noises
  in
  let new_price =
    Map.find_exn new_prices "PLTR"
    |> Money.cents_to_float_dollars
  in
  assert_bool "Price moved upward for positive noise" Float.(new_price > 10.0)

let engine_tick_test _ctx =
  let asset : Model.asset =
    {
      ticker = "TSLA";
      process = Model.GBM (Gbm.create ~mu:0.0 ~sigma:0.0 ~dt:1.0);
      initial_price = Money.float_dollars_to_cents 50.0;
    }
  in
  let universe = Model.add_asset Model.empty_universe asset in
  let config =
    {
      Engine.universe;
      initial_prices = String.Map.empty;
      initial_cash = Money.float_dollars_to_cents 1_000.0;
    }
  in
  let engine = Engine.create config in
  let noise = String.Map.singleton "TSLA" 0.0 in
  let engine' = Engine.tick engine ~noise in
  let time_advanced = Engine.time_index engine' = Engine.time_index engine + 1 in
  let prices = Engine.prices engine' in
  let price =
    Map.find_exn prices "TSLA"
    |> Money.cents_to_float_dollars
  in
  assert_bool "Time advanced by one tick" time_advanced;
  assert_bool "Price unchanged with zero volatility" (approx_equal price 50.0)

let suite =
  "pixel_portfolio_tests" >::: [
    "gbm_step" >:: gbm_step_test;
    "model_step" >:: model_step_test;
    "engine_tick" >:: engine_tick_test;
  ]

let () = run_test_tt_main suite

