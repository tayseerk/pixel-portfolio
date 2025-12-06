open Core
open OUnit2
open Pixel_portfolio_lib
open QCheck

let test_gbm_step_positive_and_capped _ =
  let m = Gbm.create ~mu:0.1 ~sigma:0.5 ~dt:1.0 in
  let price = 100.0 in
  let noise = 5.0 in
  let next = Gbm.step m ~price ~noise in
  assert_bool "Positive" Float.(next > 0.0);
  assert_bool "Capped up to ~20%" Float.(next <= 122.2 +. 1e-6);
  let noise_down = -5.0 in
  let next_down = Gbm.step m ~price ~noise:noise_down in
  assert_bool "Capped down to ~80%" Float.(next_down >= 80.0 -. 1e-6)

let test_ou_step_positive_and_capped _ =
  let m = Ou.create ~kappa:0.5 ~theta:1.0 ~sigma:0.5 ~dt:1.0 in
  let state = 10.0 in
  let noise = 5.0 in
  let next = Ou.step m ~state ~noise in
  assert_bool "Positive" Float.(next > 0.0);
  assert_bool "Capped delta" Float.(next <= state *. 1.2 +. 1e-6);
  let noise_down = -5.0 in
  let next_down = Ou.step m ~state ~noise:noise_down in
  assert_bool "Capped delta down" Float.(next_down >= state *. 0.8 -. 1e-6)

let test_model_step_universe_initial_price_and_noise _ =
  let asset =
    {
      Model.ticker = "AAPL";
      process = Model.GBM (Gbm.create ~mu:0.0 ~sigma:0.0 ~dt:1.0);
      initial_price = Money.float_dollars_to_cents 10.0;
    }
  in
  let universe = Model.add_asset Model.empty_universe asset in
  let prices = String.Map.empty in
  let noises = String.Map.of_alist_exn [ ("AAPL", 0.0) ] in
  let stepped = Model.step_universe universe ~current_prices:prices ~noises in
  match Map.find stepped "AAPL" with
  | Some px -> assert_equal (Money.float_dollars_to_cents 10.0) px
  | None -> assert_failure "Missing price"

let test_portfolio_buy_sell _ =
  let px = Money.float_dollars_to_cents 10.0 in
  let engine =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.of_alist_exn [ ("AAPL", px) ];
        initial_cash = Money.float_dollars_to_cents 1000.0;
      }
  in
  let initial_port = Engine.portfolio engine in
  let order_buy =
    Order.create_market ~ticker:"AAPL" ~type_of_order:Order.Buy ~quantity:5 ?id:None
  in
  let eng_after_buy, _ = Engine.submit_order engine ~order:order_buy in
  let port = Engine.portfolio eng_after_buy in
  assert_bool "Cash decreased" (port.cash < initial_port.cash);
  let order_sell =
    Order.create_market ~ticker:"AAPL" ~type_of_order:Order.Sell ~quantity:5 ?id:None
  in
  let eng_after_sell, _ = Engine.submit_order eng_after_buy ~order:order_sell in
  let port2 = Engine.portfolio eng_after_sell in
  assert_bool "Position removed"
    (Option.is_none (Portfolio.position_for port2 "AAPL"))

let test_limit_order_not_filled_until_price_hits _ =
  let px = Money.float_dollars_to_cents 100.0 in
  let engine =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.of_alist_exn [ ("AAPL", px) ];
        initial_cash = Money.float_dollars_to_cents 1000.0;
      }
  in
  let order_limit =
    Order.create_limit ~ticker:"AAPL" ~type_of_order:Order.Buy ~quantity:1
      ~limit_price:(Money.float_dollars_to_cents 90.0) ?id:None
  in
  let eng_after, exec = Engine.submit_order engine ~order:order_limit in
  assert_bool "Not filled immediately" (Option.is_none exec);
  let prices = String.Map.of_alist_exn [ ("AAPL", Money.float_dollars_to_cents 85.0) ] in
  let eng_after_tick =
    Engine.with_prices eng_after prices
    |> Engine.reconcile_open_orders
  in
  assert_bool "Filled after price drop"
    (List.is_empty (Engine.open_orders eng_after_tick))

let test_sell_no_position _ =
  let px = Money.float_dollars_to_cents 10.0 in
  let engine =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.of_alist_exn [ ("AAPL", px) ];
        initial_cash = Money.float_dollars_to_cents 1000.0;
      }
  in
  let order_sell =
    Order.create_market ~ticker:"AAPL" ~type_of_order:Order.Sell ~quantity:1 ?id:None
  in
  let eng_after, _ = Engine.submit_order engine ~order:order_sell in
  let port = Engine.portfolio eng_after in
  assert_bool "Still no positions after selling none"
    (List.is_empty (Portfolio.all_positions port))

let test_tick_advances_time _ =
  let px = Money.float_dollars_to_cents 10.0 in
  let engine =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.of_alist_exn [ ("AAPL", px) ];
        initial_cash = Money.float_dollars_to_cents 1000.0;
      }
  in
  let eng' = Engine.tick engine ~noise:String.Map.empty in
  assert_equal 1 (Engine.time_index eng')

let test_reconcile_idempotent _ =
  let px = Money.float_dollars_to_cents 100.0 in
  let engine =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.of_alist_exn [ ("AAPL", px) ];
        initial_cash = Money.float_dollars_to_cents 1000.0;
      }
  in
  let order_limit =
    Order.create_limit ~ticker:"AAPL" ~type_of_order:Order.Buy ~quantity:1
      ~limit_price:(Money.float_dollars_to_cents 90.0) ?id:None
  in
  let eng_after, _ = Engine.submit_order engine ~order:order_limit in
  let once = Engine.reconcile_open_orders eng_after in
  let twice = Engine.reconcile_open_orders once in
  assert_equal (Engine.open_orders once) (Engine.open_orders twice)

let test_save_load_roundtrip _ =
  let px = Money.float_dollars_to_cents 10.0 in
  let engine =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.of_alist_exn [ ("AAPL", px) ];
        initial_cash = Money.float_dollars_to_cents 1000.0;
      }
  in
  let tmp = Stdlib.Filename.temp_file "pp_state" ".sexp" in
  (match Save_game_state.save_sexp ~filename:tmp engine with
   | Pass _ -> ()
   | Fail msg -> assert_failure msg);
  match Save_game_state.load_sexp ~filename:tmp with
  | Ok eng' ->
      assert_equal (Engine.time_index engine) (Engine.time_index eng');
      assert_bool "Prices equal"
        (Map.equal Int.equal (Engine.prices engine) (Engine.prices eng'));
      assert_equal (Engine.portfolio engine).cash (Engine.portfolio eng').cash
  | Error msg -> assert_failure msg


let qcheck_gbm_positive_capped =
  Test.make
    (float_range (-5.) 5.)
    (fun noise ->
       let m = Gbm.create ~mu:0.1 ~sigma:0.5 ~dt:1.0 in
       let price = 100.0 in
       let next = Gbm.step m ~price ~noise in
       let upper = price *. Float.exp 0.2 +. 1e-6 in
       Float.(next > 0.0 && next <= upper))


let qcheck_ou_positive_capped =
  Test.make
    (pair (float_range 1. 20.) (float_range (-5.) 5.))
    (fun (state, noise) ->
       let m = Ou.create ~kappa:0.5 ~theta:1.0 ~sigma:0.5 ~dt:1.0 in
       let next = Ou.step m ~state ~noise in
       let max_step = 0.2 *. state in
       let upper = state +. max_step +. 1e-6 in
       let lower = state -. max_step -. 1e-6 in
       Float.(next > 0.0 && next <= upper && next >= lower))

let suite =
  "pixel_portfolio"
  >::: [
         "gbm_positive_capped" >:: test_gbm_step_positive_and_capped;
         "ou_positive_capped" >:: test_ou_step_positive_and_capped;
         "model_step_universe" >:: test_model_step_universe_initial_price_and_noise;
         "portfolio_buy_sell" >:: test_portfolio_buy_sell;
         "limit_fill_on_price" >:: test_limit_order_not_filled_until_price_hits;
         "save_load_roundtrip" >:: test_save_load_roundtrip;
         "sell_no_position" >:: test_sell_no_position;
         "tick_advances_time" >:: test_tick_advances_time;
        "keeps_open_orders" >:: test_reconcile_idempotent;
        "qcheck_gbm_positive_capped"
        >::: [ QCheck_ounit.to_ounit2_test qcheck_gbm_positive_capped ];
        "qcheck_ou_positive_capped"
        >::: [ QCheck_ounit.to_ounit2_test qcheck_ou_positive_capped ];
       ]

let () = run_test_tt_main suite

