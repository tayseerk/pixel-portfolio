open Core
open OUnit2
open Pixel_portfolio_lib
open QCheck

(* Test: GBM step stays positive and respects the cap on daily move.
   We step once with high positive/negative noise and assert the next price
   remains > 0 and within the capped move bounds. *)
let test_gbm_step_positive_and_capped _ =
  let m = Gbm.create ~mu:0.1 ~sigma:0.5 ~dt:1.0 in
  let price = 100.0 in
  let noise = 5.0 in
  let next = Gbm.step m ~price ~noise in
  assert_bool "Positive" Float.(next > 0.0);
  assert_bool "Capped up to ~8%" Float.(next <= 108.4 +. 1e-6);
  let noise_down = -5.0 in
  let next_down = Gbm.step m ~price ~noise:noise_down in
  assert_bool "Capped down to ~80%" Float.(next_down >= 80.0 -. 1e-6)

(* Test: OU step stays positive and respects the cap on daily delta.
   We step once with high positive/negative noise and assert the next state
   remains > 0 and within the capped percentage change. *)
let test_ou_step_positive_and_capped _ =
  let m = Ou.create ~kappa:0.5 ~theta:1.0 ~sigma:0.5 ~dt:1.0 in
  let state = 10.0 in
  let noise = 5.0 in
  let next = Ou.step m ~price:state ~noise in
  assert_bool "Positive" Float.(next > 0.0);
  assert_bool "Capped delta" Float.(next <= state *. 1.4 +. 1e-6);
  let noise_down = -5.0 in
  let next_down = Ou.step m ~price:state ~noise:noise_down in
  assert_bool "Capped delta down" Float.(next_down >= state *. 0.6 -. 1e-6)

(* Test: model.step_universe uses initial price when no current price exists and applies noise.
   We build a single-asset universe with a zero-noise GBM and assert the stepped
   price matches the initial price. *)
let test_model_step_universe_initial_price_and_noise _ =
  let asset =
    {
      Model.ticker = Ticker.of_string "AAPL";
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

(* Test: buying and then selling removes the position and cash decreases on buy.
   We submit a market buy then a sell for the same ticker, assert cash went down
   on buy, and the position is removed after the sell. *)
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
    Order.create_market ~ticker:(Ticker.of_string "AAPL") ~type_of_order:Order.Buy ~quantity:5 ?id:None
  in
  let eng_after_buy, _ = Engine.submit_order engine ~order:order_buy in
  let port = Engine.portfolio eng_after_buy in
  assert_bool "Cash decreased" (Money.compare port.cash initial_port.cash < 0);
  let order_sell =
    Order.create_market ~ticker:(Ticker.of_string "AAPL") ~type_of_order:Order.Sell ~quantity:5 ?id:None
  in
  let eng_after_sell, _ = Engine.submit_order eng_after_buy ~order:order_sell in
  let port2 = Engine.portfolio eng_after_sell in
  assert_bool "Position removed"
    (Option.is_none (Portfolio.position_for port2 (Ticker.of_string "AAPL")))


(* Test: limit order remains open until price crosses limit, then fills on reconcile.
   We place a limit buy below market, assert it’s open, drop the price, reconcile,
   and assert the order book is empty. *)
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
    Order.create_limit ~ticker:(Ticker.of_string "AAPL") ~type_of_order:Order.Buy ~quantity:1
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

(* Test: tick increments the engine time index by 1.
   We call tick once and assert time_index increases to 1. *)
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

(* Test: reconciling open orders twice yields the same result (idempotent).
   We submit one limit order, reconcile twice, and assert open_orders is unchanged. *)
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
    Order.create_limit ~ticker:(Ticker.of_string "AAPL") ~type_of_order:Order.Buy ~quantity:1
      ~limit_price:(Money.float_dollars_to_cents 90.0) ?id:None
  in
  let eng_after, _ = Engine.submit_order engine ~order:order_limit in
  let once = Engine.reconcile_open_orders eng_after in
  let twice = Engine.reconcile_open_orders once in
  assert_equal (Engine.open_orders once) (Engine.open_orders twice)

(* Test: saving and loading preserves time index, prices, and cash.
   We save to a temp file, load it back, and compare time_index, prices, and cash. *)
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
        (Map.equal (fun x y -> Money.compare x y = 0)
          (Engine.prices engine)
          (Engine.prices eng'));
      assert_equal (Engine.portfolio engine).cash (Engine.portfolio eng').cash
  | Error msg -> assert_failure msg

(* Test: money arithmetic, equality, and formatting helpers.
   We check add/subtract/multiply, equality, and the string formatter for positive/negative. *)
let test_money_ops_and_format _ =
  let open Money in
  let a = of_int_cents 150 in
  let b = of_int_cents 50 in
  assert_equal 200 (to_int_cents (a $+ b));
  assert_equal 100 (to_int_cents (a $- b));
  assert_equal 300 (to_int_cents (multiply a 2));
  assert_bool "equal works" (equal a (of_int_cents 150));
  assert_equal "1.50" (make_it_look_nice a);
  assert_equal "-1.50" (make_it_look_nice (of_int_cents (-150)))

(* Test: noise.sample stays bounded and ensure_noise_map preserves existing and adds missing.
   We sample multiple times to ensure bound, then fill a noise map keeping one entry
   and adding a missing ticker. *)
let test_noise_sample_and_fill _ =
  for _i = 1 to 50 do
    let n = Noise.sample () in
    assert_bool "noise bounded" (Float.(abs n <= 0.2))
  done;
  let universe =
    Model.add_asset Model.empty_universe
      { Model.ticker = Ticker.of_string "AAPL";
        process = Model.GBM (Gbm.create ~mu:0.0 ~sigma:0.0 ~dt:1.0);
        initial_price = Money.of_int_cents 100 }
  in
  let existing = String.Map.of_alist_exn [ ("AAPL", 0.5) ] in
  let filled = Noise.ensure_noise_map ~universe ~existing in
  let existing_val = Map.find_exn filled "AAPL" in
  assert_bool "keeps existing" Float.(existing_val = 0.5);
  let universe2 =
    Model.add_asset universe
      { Model.ticker = Ticker.of_string "MSFT";
        process = Model.GBM (Gbm.create ~mu:0.0 ~sigma:0.0 ~dt:1.0);
        initial_price = Money.of_int_cents 100 }
  in
  let filled2 = Noise.ensure_noise_map ~universe:universe2 ~existing in
  assert_bool "adds missing"
    (Map.mem filled2 "MSFT")

(* Test: stop-loss order creation and status transitions (cancel/filled).
   We build a stop-loss, assert its kind, then mark it cancelled and filled and check statuses. *)
let test_order_stop_loss_and_cancel _ =
  let ticker = Ticker.of_string "AAPL" in
  let o =
    Order.create_stop_loss ~ticker ~type_of_order:Order.Sell ~quantity:1
      ~stop_price:(Money.of_int_cents 50) ?id:None
  in
  (match o.Order.kind with
   | Order.Stop_loss _ -> ()
   | _ -> assert_failure "Expected stop_loss");
  let cancelled = Order.order_cancelled o in
  assert_equal Order.Cancelled cancelled.Order.status;
  let filled = Order.order_filled o in
  assert_equal Order.Filled filled.Order.status

(* Test: short a position and then buy enough to flip to a long position.
   We open a short, verify cash increased, then buy more than the short size and assert a long remains. *)
let test_portfolio_short_and_flip _ =
  let open Money in
  let px = of_int_cents 1000 in
  let portfolio =
    Portfolio.of_cash (of_int_cents 10_000)
    |> Portfolio.update_position
         ~ticker:(Ticker.of_string "XYZ")
         ~side:Order.Sell ~quantity:5 ~fill_price:px
  in
  (* Sell short increases cash and creates short position *)
  assert_bool "cash increased" (compare portfolio.Portfolio.cash (of_int_cents 10_000) > 0);
  let flipped =
    Portfolio.update_position portfolio
      ~ticker:(Ticker.of_string "XYZ")
      ~side:Order.Buy ~quantity:10 ~fill_price:(of_int_cents 500)
  in
  match Portfolio.position_for flipped (Ticker.of_string "XYZ") with
  | Some pos ->
      assert_bool "now long" (phys_equal pos.direction Portfolio.Long);
      assert_equal 5 pos.quantity;
      assert_equal (of_int_cents 500) pos.avg_cost
  | None -> assert_failure "Expected a long position"

(* Test: level increases when equity grows enough (price appreciation).
   We buy a share, bump its price, reconcile, and assert the player level rises. *)
let test_engine_level_up _ =
  let px = Money.float_dollars_to_cents 100.0 in
  let engine =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.of_alist_exn [ ("AAPL", px) ];
        initial_cash = Money.float_dollars_to_cents 1000.0;
      }
  in
  let order_buy =
    Order.create_market ~ticker:(Ticker.of_string "AAPL") ~type_of_order:Order.Buy ~quantity:1 ?id:None
  in
  let eng_after_buy, _ = Engine.submit_order engine ~order:order_buy in
  let richer =
    Engine.with_prices eng_after_buy
      (String.Map.of_alist_exn [ ("AAPL", Money.float_dollars_to_cents 200.0) ])
    |> Engine.reconcile_open_orders
  in
  assert_bool "level increased" (Engine.level richer >= 3)

(* Test: cancel_order removes the targeted open order.
   We add a limit order, cancel by id, and assert open_orders is empty. *)
let test_engine_cancel_order _ =
  let px = Money.float_dollars_to_cents 50.0 in
  let engine =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.of_alist_exn [ ("AAPL", px) ];
        initial_cash = Money.float_dollars_to_cents 1000.0;
      }
  in
  let order_limit =
    Order.create_limit ~ticker:(Ticker.of_string "AAPL") ~type_of_order:Order.Buy ~quantity:1
      ~limit_price:px ?id:None
  in
  let eng_after, _ = Engine.submit_order engine ~order:order_limit in
  let id = order_limit.Order.id in
  let cancelled = Engine.cancel_order eng_after id in
  assert_bool "order removed" (List.is_empty (Engine.open_orders cancelled))

(* Test: step_universe prefers current prices over initial prices when provided.
   We give a current price different from initial and assert the stepped price matches current. *)
let test_model_step_prefers_current_price _ =
  let asset =
    {
      Model.ticker = Ticker.of_string "AAPL";
      process = Model.GBM (Gbm.create ~mu:0.0 ~sigma:0.0 ~dt:1.0);
      initial_price = Money.float_dollars_to_cents 10.0;
    }
  in
  let universe = Model.add_asset Model.empty_universe asset in
  let prices = String.Map.of_alist_exn [ ("AAPL", Money.float_dollars_to_cents 20.0) ] in
  let noises = String.Map.of_alist_exn [ ("AAPL", 0.0) ] in
  let stepped = Model.step_universe universe ~current_prices:prices ~noises in
  match Map.find stepped "AAPL" with
  | Some px -> assert_equal (Money.float_dollars_to_cents 20.0) px
  | None -> assert_failure "Missing price"

let qcheck_gbm_positive_capped =
  Test.make
    (float_range (-5.) 5.)
    (fun noise ->
       let m = Gbm.create ~mu:0.1 ~sigma:0.5 ~dt:1.0 in
       let price = 100.0 in
       let next = Gbm.step m ~price ~noise in
       let upper = price *. Float.exp 0.08 +. 1e-6 in
       Float.(next > 0.0 && next <= upper))


let qcheck_ou_positive_capped =
  Test.make
    (pair (float_range 1. 20.) (float_range (-5.) 5.))
    (fun (state, noise) ->
       let m = Ou.create ~kappa:0.5 ~theta:1.0 ~sigma:0.5 ~dt:1.0 in
       let next = Ou.step m ~price:state ~noise in
       let max_step = 0.4 *. state in
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
         "tick_advances_time" >:: test_tick_advances_time;
        "keeps_open_orders" >:: test_reconcile_idempotent;
        "money_ops" >:: test_money_ops_and_format;
        "noise_sample" >:: test_noise_sample_and_fill;
        "order_stop_loss_cancel" >:: test_order_stop_loss_and_cancel;
        "portfolio_short_flip" >:: test_portfolio_short_and_flip;
        "engine_level_up" >:: test_engine_level_up;
        "engine_cancel" >:: test_engine_cancel_order;
        "model_prefers_current_price" >:: test_model_step_prefers_current_price;
        "qcheck_gbm_positive_capped"
        >::: [ QCheck_ounit.to_ounit2_test qcheck_gbm_positive_capped ];
        "qcheck_ou_positive_capped"
        >::: [ QCheck_ounit.to_ounit2_test qcheck_ou_positive_capped ];
       ]

let () = run_test_tt_main suite

