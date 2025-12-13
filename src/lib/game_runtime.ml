open Core

module View = Game_view

(* Value: upper bound on simulated steps per command *)
let max_sim_steps = 160

(* Function: build a fresh engine configured for the selected difficulty *)
let create_engine difficulty =
  let config =
    {
      Engine.universe = Game_config.base_universe;
      initial_prices = String.Map.empty;
      initial_cash = Game_config.starting_cash difficulty;
    }
  in
  Engine.create config

(* ----- IO helpers ----- *)

(* Function: copy file contents from source to destination *)
let copy_file ~src ~dst =
  Or_error.try_with (fun () ->
      let data = In_channel.read_all src in
      Out_channel.write_all dst ~data)

(* Function: write backup file if the source exists *)
let backup_if_exists path =
  if Stdlib.Sys.file_exists path then
    let backup = path ^ ".bak" in
    copy_file ~src:path ~dst:backup
    |> Or_error.map ~f:(fun () ->
           printf "Existing state saved to %s\n%!" backup)
  else
    Or_error.return ()

(* Function: save engine state to a sexp file *)
let save_engine ~state engine =
  match Save_game_state.save_sexp ~filename:state engine with
  | Save_game_state.Pass _ -> Or_error.return ()
  | Fail msg -> Or_error.error_string msg

(* Function: load state, reconcile orders, and rewrite to memory *)
let load_engine ~state =
  let open Or_error.Let_syntax in
  if Stdlib.Sys.file_exists state then
    match Save_game_state.load_sexp ~filename:state with
    | Ok engine ->
        let engine' = Engine.reconcile_open_orders engine in
        let%map () = save_engine ~state engine' in
        engine'
    | Error msg -> Or_error.error_string msg
  else
    Or_error.errorf "No saved game at %s. Run new first." state

(* Function: verify a file exists before using it *)
let ensure_source_exists path =
  if Stdlib.Sys.file_exists path then Or_error.return ()
  else Or_error.errorf "File %s does not exist." path

(* Function: run N ticks, generating noise internally for each ticker *)
let rec advance_ticks engine steps =
  if steps <= 0 then engine
  else
    let next_engine = Engine.tick engine ~noise:String.Map.empty in
    advance_ticks next_engine (steps - 1)

(* Function: validate and convert optional dollars to cents *)
let price_arg_to_cents = function
  | None -> Or_error.return None
  | Some dollars ->
      if Float.(dollars <= 0.) then
        Or_error.errorf "Price must be positive, got %f" dollars
      else
        Or_error.return (Some (Money.of_float_dollars dollars))

(* Function: reject non-positive quantities *)
let qty_must_be_positive qty =
  if qty <= 0 then Or_error.errorf "Quantity must be positive, got %d" qty
  else Or_error.return ()

(* Function: check cash before a buy order *)
let ensure_can_afford_buy ~engine ~ticker ~qty ~price_cents =
  let open Or_error.Let_syntax in
  let open Money in
  let cost = multiply price_cents qty in
  let cash = (Engine.portfolio engine).Portfolio.cash in
  if compare cost cash > 0 then
    Or_error.errorf
      "Insufficient cash to buy %d share(s) of %s: need $%s but only have $%s"
      qty
      ticker
      (View.pretty_money cost)
      (View.pretty_money cash)
  else
    return ()

(* Function: check that a SELL is allowed (no naked/oversell/short-expand) *)
let ensure_can_sell ~engine ~ticker ~qty =
  let open Or_error.Let_syntax in
  let portfolio = Engine.portfolio engine in
  match Portfolio.position_for portfolio (Ticker.of_string ticker) with
  | None ->
      Or_error.errorf
        "Cannot sell %d share(s) of %s: no open position."
        qty ticker
  | Some pos ->
      (match pos.Portfolio.direction with
       | Portfolio.Short ->
           Or_error.errorf
             "Cannot place a SELL order on an existing short position in %s. Use BUY to cover."
             ticker
       | Portfolio.Long ->
           if qty > pos.Portfolio.quantity then
             Or_error.errorf
               "Cannot sell %d share(s) of %s: only %d share(s) available."
               qty ticker pos.Portfolio.quantity
           else
             return ())

(* Function: build order, save state, and report status *)
let place_order ~state ~ticker ~qty ~price_opt ~side =
  let open Or_error.Let_syntax in
  let ticker_sym = ticker in                 (* string, e.g. "AAPL" *)
  let ticker_t = Ticker.of_string ticker_sym in  (* Ticker.t *)
  let%bind () = qty_must_be_positive qty in
  let%bind limit_or_stop = price_arg_to_cents price_opt in
  let%bind engine = load_engine ~state in
  (* Margin / consistency checks before order creation *)
  let%bind () =
    match (side, limit_or_stop) with
    | Order.Buy, Some px ->
        ensure_can_afford_buy ~engine ~ticker:ticker_sym ~qty ~price_cents:px
    | Order.Buy, None -> (
        match Map.find (Engine.prices engine) ticker_sym with
        | None ->
            Or_error.errorf "No price available for ticker %s" ticker_sym
        | Some px ->
            ensure_can_afford_buy ~engine ~ticker:ticker_sym ~qty ~price_cents:px)
    | Order.Sell, _ ->
        ensure_can_sell ~engine ~ticker:ticker_sym ~qty
  in
  let order =
    match (side, limit_or_stop) with
    | _, None ->
        Order.create_market
          ~ticker:ticker_t ~type_of_order:side ~quantity:qty ?id:None
    | Order.Buy, Some px ->
        Order.create_limit
          ~ticker:ticker_t ~type_of_order:side ~quantity:qty
          ~limit_price:px ?id:None
    | Order.Sell, Some px ->
        Order.create_stop_loss
          ~ticker:ticker_t ~type_of_order:side ~quantity:qty
          ~stop_price:px ?id:None
  in
  let engine', exec_opt = Engine.submit_order engine ~order in
  let%bind () = save_engine ~state engine' in
  (match exec_opt with
   | Some exec ->
       printf "%s order filled at $%s\n%!"
         (match side with Buy -> "Buy" | Sell -> "Sell")
         (View.pretty_money exec.Order.fill_price)
   | None ->
       printf
         "%s order recorded as an open order.\n%!"
         (match side with Buy -> "Buy" | Sell -> "Sell"));
  View.print_positions engine';
  return ()

(* Function: advance N steps, save, and show price deltas *)
let simulate ~state ~steps =
  let open Or_error.Let_syntax in
  if steps <= 0 then
    Or_error.errorf "Steps must be >= 1, got %d" steps
  else if steps > max_sim_steps then
    Or_error.errorf
      "Steps must be between 1 and %d, got %d"
      max_sim_steps steps
  else
    let%bind engine = load_engine ~state in
    let old_prices = Engine.prices engine in
    let final_engine = advance_ticks engine steps in
    let%bind () = save_engine ~state final_engine in
    printf "Simulated %d step(s). Current time index: %d\n"
      steps (Engine.time_index final_engine);
    let new_prices = Engine.prices final_engine in
    Map.merge old_prices new_prices ~f:(fun ~key:ticker -> function
        | `Both (old_price, new_price) ->
          let delta = Money.(new_price $- old_price) in
          let pct =
            if Money.to_int_cents old_price = 0 then 0.0
            else
              let d = Money.to_float_dollars delta in
              let o = Money.to_float_dollars old_price in
              (d /. o) *. 100.0
          in
          printf "  %s: $%s -> $%s (%s%s, %.2f%%)\n"
            ticker
            (View.pretty_money old_price)
            (View.pretty_money new_price)
            (if Money.to_int_cents delta >= 0 then "+" else "")
            (View.pretty_money delta)
            pct;
          None
      | _ -> None)
    |> ignore;
    View.print_positions final_engine;
    return ()

(* Function: start a new game, backup old state, show status *)
let new_game ~state ~mode =
  let open Or_error.Let_syntax in
  let%bind () = backup_if_exists state in
  let engine = create_engine mode in
  let%bind () = save_engine ~state engine in
  printf "Started new %s game at %s.\n"
    (Game_config.to_string mode)
    state;
  View.print_positions engine;
  View.print_prices engine;
  return engine

(* Function: copy current state file to a new target *)
let save_as ~state ~target =
  let open Or_error.Let_syntax in
  let%bind () = ensure_source_exists state in
  let%bind engine = load_engine ~state in
  let%bind () = save_engine ~state:target engine in
  printf "Saved current game to %s\n%!" target;
  return ()

(* Function: load from source into state, backing up current *)
let load_into ~state ~source =
  let open Or_error.Let_syntax in
  let%bind () = ensure_source_exists source in
  let%bind () = backup_if_exists state in
  let%bind engine =
    match Save_game_state.load_sexp ~filename:source with
    | Ok eng -> Or_error.return eng
    | Error msg -> Or_error.error_string msg
  in
  let%bind () = save_engine ~state engine in
  printf "Loaded state from %s into %s\n%!" source state;
  View.print_positions engine;
  View.print_prices engine;
  return engine

(* Function: cancel an open order by id and save the state *)
let cancel_order_cmd ~state ~order_id =
  let open Or_error.Let_syntax in
  let%bind engine = load_engine ~state in
  let before = Engine.open_orders engine in
  let engine' = Engine.cancel_order engine order_id in
  let after = Engine.open_orders engine' in
  let%bind () = save_engine ~state engine' in
  if List.length before = List.length after then
    printf "No open order with id #%d found.\n%!" order_id
  else
    printf "Cancelled order #%d.\n%!" order_id;
  return ()
