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

(* Test: GBM simulate_path generates a path over multiple noise steps.
   Verifies that simulate_path produces output of correct length and all prices are positive. *)
let test_gbm_simulate_path _ =
  let model = Gbm.create ~mu:0.05 ~sigma:0.2 ~dt:1.0 in
  let initial_price = 100.0 in
  let noises = [ 0.0; 0.5; -0.3; 0.1 ] in
  let path = Gbm.simulate_path model ~initial_price ~noises in
  assert_equal 4 (List.length path);
  List.iter path ~f:(fun price ->
      assert_bool "all prices positive" Float.(price > 0.0))

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

(* Test: game_config round-trips difficulty strings and cash per difficulty. *)
let test_game_config_roundtrip _ =
  assert_equal (Some Game_config.Easy) (Game_config.of_string "easy");
  assert_equal (Some Game_config.Medium) (Game_config.of_string "medium");
  assert_equal (Some Game_config.Hard) (Game_config.of_string "hard");
  assert_bool "invalid difficulty" (Option.is_none (Game_config.of_string "nope"));
  let cash_easy = Game_config.starting_cash Game_config.Easy in
  let cash_med = Game_config.starting_cash Game_config.Medium in
  let cash_hard = Game_config.starting_cash Game_config.Hard in
  assert_bool "cash easy > medium" (Money.compare cash_easy cash_med > 0);
  assert_bool "cash medium > hard" (Money.compare cash_med cash_hard > 0)

(* EDGE CASE TEST: save_game_state fails when path is unwritable, and load fails on bad content. *)
let test_save_game_state_error_paths _ =
  let engine =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.empty;
        initial_cash = Money.of_int_cents 0;
      }
  in
  let dir_path = Stdlib.Filename.get_temp_dir_name () in
  (match Save_game_state.save_sexp ~filename:dir_path engine with
   | Fail _ -> ()
   | Pass _ -> assert_failure "Expected save_sexp to fail for directory path");
  let bad = Stdlib.Filename.temp_file "pp_bad" ".sexp" in
  Out_channel.write_all bad ~data:"this is not a sexp";
  (match Save_game_state.load_sexp ~filename:bad with
   | Ok _ -> assert_failure "Expected load_sexp to fail on bad content"
   | Error _ -> ())

(* Test: order helpers create correct kinds/status and next_id increments. *)
let test_order_constructors_and_status _ =
  let ticker = Ticker.of_string "AAPL" in
  let limit =
    Order.create_limit ~ticker ~type_of_order:Order.Buy ~quantity:1
      ~limit_price:(Money.of_int_cents 100) ?id:None
  in
  (match limit.Order.kind with
   | Order.Limit _ -> ()
   | _ -> assert_failure "Expected Limit");
  let stop =
    Order.create_stop_loss ~ticker ~type_of_order:Order.Sell ~quantity:1
      ~stop_price:(Money.of_int_cents 90) ?id:None
  in
  (match stop.Order.kind with
   | Order.Stop_loss _ -> ()
   | _ -> assert_failure "Expected Stop_loss");
  assert_equal Order.Cancelled (Order.order_cancelled stop).Order.status;
  assert_equal Order.Filled (Order.order_filled stop).Order.status

(* Test: Market order with Buy type to cover uncovered constructors.
   This test specifically targets the Buy and Market constructors that show as red in coverage. *)
let test_order_buy_market _ =
  let ticker = Ticker.of_string "MSFT" in
  let market_buy =
    Order.create_market ~ticker ~type_of_order:Order.Buy ~quantity:5 ?id:None
  in
  (match market_buy.Order.kind with
   | Order.Market -> ()
   | _ -> assert_failure "Expected Market");
  assert_equal Order.Buy market_buy.Order.type_of_order

(* Test: portfolio short/long branches — flip to short, grow short, partial/complete closes. *)
let test_portfolio_short_and_close_paths _ =
  let open Money in
  let px = of_int_cents 1000 in
  let portfolio =
    Portfolio.of_cash (of_int_cents 10_000)
    |> Portfolio.update_position
         ~ticker:(Ticker.of_string "XYZ")
         ~side:Order.Sell ~quantity:5 ~fill_price:px
  in
  let grown =
    Portfolio.update_position portfolio
      ~ticker:(Ticker.of_string "XYZ")
      ~side:Order.Sell ~quantity:5 ~fill_price:(of_int_cents 500)
  in
  (match Portfolio.position_for grown (Ticker.of_string "XYZ") with
   | Some pos ->
       assert_bool "still short" (phys_equal pos.direction Portfolio.Short);
       assert_equal 10 pos.quantity
   | None -> assert_failure "Expected short position");
  let long_port =
    Portfolio.of_cash (of_int_cents 5_000)
    |> Portfolio.update_position
         ~ticker:(Ticker.of_string "ABC")
         ~side:Order.Buy ~quantity:10 ~fill_price:(of_int_cents 100)
  in
  let partial =
    Portfolio.update_position long_port
      ~ticker:(Ticker.of_string "ABC")
      ~side:Order.Sell ~quantity:4 ~fill_price:(of_int_cents 150)
  in
  (match Portfolio.position_for partial (Ticker.of_string "ABC") with
   | Some pos -> assert_equal 6 pos.quantity
   | None -> assert_failure "Expected remaining long");
  let closed =
    Portfolio.update_position partial
      ~ticker:(Ticker.of_string "ABC")
      ~side:Order.Sell ~quantity:6 ~fill_price:(of_int_cents 150)
  in
  assert_bool "long closed"
    (Option.is_none (Portfolio.position_for closed (Ticker.of_string "ABC")))

(* Test: portfolio market_value handles missing prices and short sign. *)
let test_portfolio_market_value_long_short_and_missing _ =
  let open Money in
  let port =
    Portfolio.of_cash (of_int_cents 0)
    |> Portfolio.update_position
         ~ticker:(Ticker.of_string "LNG")
         ~side:Order.Buy ~quantity:2 ~fill_price:(of_int_cents 100)
    |> Portfolio.update_position
         ~ticker:(Ticker.of_string "SRT")
         ~side:Order.Sell ~quantity:3 ~fill_price:(of_int_cents 50)
  in
  let prices =
    String.Map.of_alist_exn
      [ ("LNG", of_int_cents 200) ]
  in
  let mv = Portfolio.market_value ~prices port in
  assert_equal (of_int_cents (200 * 2)) mv

(* Test: with_cash setter sets new cash. *)
let test_portfolio_with_cash _ =
  let open Money in
  let port = Portfolio.of_cash (of_int_cents 100) in
  let updated = Portfolio.with_cash port (of_int_cents 500) in
  assert_equal (of_int_cents 500) updated.Portfolio.cash

(* Test: game_runtime simulate happy paths (1 step and max_sim_steps). *)
let test_runtime_simulate_happy_paths _ =
  let state = Stdlib.Filename.temp_file "pp_sim_ok" ".sexp" in
  ignore (Game_runtime.new_game ~state ~mode:Game_config.Easy);
  assert_bool "simulate 1 step ok" (Result.is_ok (Game_runtime.simulate ~state ~steps:1));
  assert_bool "simulate max steps ok"
    (Result.is_ok (Game_runtime.simulate ~state ~steps:Game_runtime.max_sim_steps))

(* Test: game_runtime place_order happy paths (market buy, stop-loss sell after buy). *)
let test_runtime_place_order_happy_paths _ =
  let state = Stdlib.Filename.temp_file "pp_orders_ok" ".sexp" in
  ignore (Game_runtime.new_game ~state ~mode:Game_config.Easy);
  let buy_ok =
    Game_runtime.place_order ~state ~ticker:"AAPL" ~qty:1 ~price_opt:None ~side:Order.Buy
  in
  assert_bool "market buy ok" (Result.is_ok buy_ok);
  let sell_ok =
    Game_runtime.place_order ~state ~ticker:"AAPL" ~qty:1
      ~price_opt:(Some 1.0) ~side:Order.Sell
  in
  assert_bool "stop-loss sell ok" (Result.is_ok sell_ok)

(* EDGE CASE TEST: runtime place_order error cases not yet covered. *)
let test_runtime_place_order_errors _ =
  let state = Stdlib.Filename.temp_file "pp_orders_err" ".sexp" in
  ignore (Game_runtime.new_game ~state ~mode:Game_config.Easy);
  let neg_price =
    Game_runtime.place_order ~state ~ticker:"AAPL" ~qty:1 ~price_opt:(Some (-1.0)) ~side:Order.Buy
  in
  assert_bool "negative price rejected" (Result.is_error neg_price);
  let zero_qty =
    Game_runtime.place_order ~state ~ticker:"AAPL" ~qty:0 ~price_opt:None ~side:Order.Buy
  in
  assert_bool "zero qty rejected" (Result.is_error zero_qty);
  let missing_price =
    Game_runtime.place_order ~state ~ticker:"MISSING" ~qty:1 ~price_opt:None ~side:Order.Buy
  in
  assert_bool "missing ticker price rejected" (Result.is_error missing_price)

(* Test: game_runtime load_engine error when file missing; save_as/load_into error on missing source. *)
let test_runtime_file_errors _ =
  let missing = "/tmp/definitely_missing_pp_state.sexp" in
  assert_bool "load_engine missing file errors" (Result.is_error (Game_runtime.load_engine ~state:missing));
  let state = Stdlib.Filename.temp_file "pp_state_ok" ".sexp" in
  ignore (Game_runtime.new_game ~state ~mode:Game_config.Easy);
  let missing_src = "/tmp/definitely_missing_src.sexp" in
  assert_bool "save_as errors on missing source"
    (Result.is_error (Game_runtime.save_as ~state:missing_src ~target:"/tmp/nowhere"));
  assert_bool "load_into errors on missing source"
    (Result.is_error (Game_runtime.load_into ~state ~source:missing_src))

(* Test: game_view printing branches (positive/negative deltas, empty). *)
let test_game_view_prints _ =
  let px_pos = Money.of_int_cents 100 in
  let px_neg = Money.of_int_cents 50 in
  let engine =
    let base =
      Engine.create
        {
          Engine.universe = Model.empty_universe;
          initial_prices = String.Map.of_alist_exn [ ("UP", px_pos); ("DOWN", px_pos) ];
          initial_cash = Money.of_int_cents 0;
        }
    in
    let prices = String.Map.of_alist_exn [ ("UP", Money.of_int_cents 200); ("DOWN", px_neg) ] in
    Engine.with_prices base prices
  in
  Game_view.print_prices engine;
  Game_view.print_positions engine;
  let engine_empty =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.empty;
        initial_cash = Money.of_int_cents 0;
      }
  in
  Game_view.print_prices engine_empty;
  Game_view.print_positions engine_empty


(* EDGE CASE TEST: reject sells when no position exists; expect error. *)
let test_runtime_rejects_sell_without_position _ =
  let state = Stdlib.Filename.temp_file "pp_no_pos" ".sexp" in
  ignore (Game_runtime.new_game ~state ~mode:Game_config.Easy);
  match Game_runtime.place_order ~state ~ticker:"AAPL" ~qty:1 ~price_opt:None ~side:Order.Sell with
  | Ok _ -> assert_failure "Expected sell without position to error"
  | Error _ -> () (* expected *)

(* EDGE CASE TEST: reject simulate with invalid step counts (<=0 or above max). *)
let test_runtime_simulate_bounds _ =
  let state = Stdlib.Filename.temp_file "pp_sim" ".sexp" in
  ignore (Game_runtime.new_game ~state ~mode:Game_config.Easy);
  let too_low = Game_runtime.simulate ~state ~steps:0 in
  assert_bool "steps <= 0 rejected" (Result.is_error too_low);
  let too_high = Game_runtime.simulate ~state ~steps:(Game_runtime.max_sim_steps + 1) in
  assert_bool "steps > max_sim_steps rejected" (Result.is_error too_high)

(* EDGE CASE TEST: reject unaffordable buy (price too high vs cash). *)
let test_runtime_rejects_unaffordable_buy _ =
  let state = Stdlib.Filename.temp_file "pp_cash" ".sexp" in
  ignore (Game_runtime.new_game ~state ~mode:Game_config.Easy);
  let huge_price = Some 1e9 in
  let res =
    Game_runtime.place_order ~state ~ticker:"AAPL" ~qty:1 ~price_opt:huge_price ~side:Order.Buy
  in
  assert_bool "buy rejected when cash insufficient" (Result.is_error res)

(* Test: cancel_order_cmd is harmless when id is missing (no crash, Ok result). *)
let test_runtime_cancel_missing_id _ =
  let state = Stdlib.Filename.temp_file "pp_cancel" ".sexp" in
  ignore (Game_runtime.new_game ~state ~mode:Game_config.Easy);
  let res = Game_runtime.cancel_order_cmd ~state ~order_id:999 in
  assert_bool "missing id returns Ok" (Result.is_ok res)

(* Test: Engine with_portfolio setter updates portfolio.
   Verifies that with_portfolio correctly replaces the engine's portfolio. *)
let test_engine_with_portfolio _ =
  let px = Money.float_dollars_to_cents 50.0 in
  let engine =
    Engine.create
      {
        Engine.universe = Model.empty_universe;
        initial_prices = String.Map.of_alist_exn [ ("AAPL", px) ];
        initial_cash = Money.float_dollars_to_cents 1000.0;
      }
  in
  let new_portfolio = Portfolio.of_cash (Money.float_dollars_to_cents 5000.0) in
  let updated = Engine.with_portfolio engine new_portfolio in
  assert_equal (Money.float_dollars_to_cents 5000.0) (Engine.portfolio updated).Portfolio.cash

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


(* Test: OU simulate_path generates a path over multiple noise steps.
   Verifies that simulate_path produces output of correct length and all prices are positive. *)
let test_ou_simulate_path _ =
  let model = Ou.create ~kappa:0.5 ~theta:10.0 ~sigma:0.2 ~dt:1.0 in
  let initial_state = 8.0 in
  let noises = [ 0.0; 0.5; -0.3; 0.1 ] in
  let path = Ou.simulate_path model ~initial_state ~noises in
  assert_equal 4 (List.length path);
  List.iter path ~f:(fun price ->
      assert_bool "all prices positive" Float.(price > 0.0))

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
         "gbm_simulate_path" >:: test_gbm_simulate_path;
         "ou_positive_capped" >:: test_ou_step_positive_and_capped;
         "ou_simulate_path" >:: test_ou_simulate_path;
         "model_step_universe" >:: test_model_step_universe_initial_price_and_noise;
         "portfolio_buy_sell" >:: test_portfolio_buy_sell;
         "limit_fill_on_price" >:: test_limit_order_not_filled_until_price_hits;
         "save_load_roundtrip" >:: test_save_load_roundtrip;
         "tick_advances_time" >:: test_tick_advances_time;
        "keeps_open_orders" >:: test_reconcile_idempotent;
        "money_ops" >:: test_money_ops_and_format;
        "noise_sample" >:: test_noise_sample_and_fill;
        "order_stop_loss_cancel" >:: test_order_stop_loss_and_cancel;
        "order_buy_market" >:: test_order_buy_market;
        "portfolio_short_flip" >:: test_portfolio_short_and_flip;
        "engine_level_up" >:: test_engine_level_up;
        "engine_cancel" >:: test_engine_cancel_order;
        "engine_with_portfolio" >:: test_engine_with_portfolio;
        "game_config_roundtrip" >:: test_game_config_roundtrip;
        "save_game_state_error_paths" >:: test_save_game_state_error_paths;
        "order_constructors_status" >:: test_order_constructors_and_status;
        "portfolio_short_close_paths" >:: test_portfolio_short_and_close_paths;
        "portfolio_market_value_missing" >:: test_portfolio_market_value_long_short_and_missing;
        "portfolio_with_cash" >:: test_portfolio_with_cash;
        "runtime_simulate_happy" >:: test_runtime_simulate_happy_paths;
        "runtime_place_order_happy" >:: test_runtime_place_order_happy_paths;
        "runtime_place_order_errors" >:: test_runtime_place_order_errors;
        "runtime_file_errors" >:: test_runtime_file_errors;
        "game_view_prints" >:: test_game_view_prints;
        "runtime_reject_sell_no_position" >:: test_runtime_rejects_sell_without_position;
        "runtime_simulate_bounds" >:: test_runtime_simulate_bounds;
        "runtime_reject_unaffordable_buy" >:: test_runtime_rejects_unaffordable_buy;
        "runtime_cancel_missing_id" >:: test_runtime_cancel_missing_id;
        "model_prefers_current_price" >:: test_model_step_prefers_current_price;
        "qcheck_gbm_positive_capped"
        >::: [ QCheck_ounit.to_ounit2_test qcheck_gbm_positive_capped ];
        "qcheck_ou_positive_capped"
        >::: [ QCheck_ounit.to_ounit2_test qcheck_ou_positive_capped ];
       ]

let () = run_test_tt_main suite

