open Core
open Cmdliner
open Cmdliner.Term.Syntax
open Pixel_portfolio_lib

module Difficulty = Game_config

(* ----- Game configuration ----- *)

(* Value: default state file for save and load files *)
let default_state_file = Game_config.default_state_file

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

(* Function: Money formatting for values *)
let pretty_money amount = Money.make_it_look_nice amount

(* Function: initial or configured price map for deltas(how much each ticker has moved, price difference *)
let initial_price_map (engine : Engine.t) =
  let cfg = Engine.config engine in
  if Map.is_empty cfg.initial_prices then
    Model.initial_prices cfg.universe
  else
    cfg.initial_prices

(* Function: show current prices with deltas vs initial *)
let print_prices engine =
  let current = Engine.prices engine in
  let reference = initial_price_map engine in
  printf "\nCurrent prices (t=%d):\n" (Engine.time_index engine);
  Map.iteri current ~f:(fun ~key:ticker ~data:price ->
      let ref_price =
        Map.find reference ticker
        |> Option.value ~default:price
      in
      let delta = Money.(price $- ref_price) in
      let pct =
        if Money.to_int_cents ref_price = 0 then 0.0
        else
          let d = Money.to_float_dollars delta in
          let r = Money.to_float_dollars ref_price in
          (d /. r) *. 100.0
      in
      printf "  %s: $%s (%s%s, %.2f%%)\n"
        ticker
        (pretty_money price)
        (if Money.to_int_cents delta >= 0 then "+" else "")
        (pretty_money delta)
        pct);
  printf "%!"

(* Function: run N ticks, generating noise internally for each ticker *)
let rec advance_ticks engine steps =
  if steps <= 0 then engine
  else
    let next_engine = Engine.tick engine ~noise:String.Map.empty in
    advance_ticks next_engine (steps - 1)

(* Function: show cash, equity, positions, open orders *)
let print_positions engine =
  let portfolio = Engine.portfolio engine in
  printf "\nCash: $%s\n" (pretty_money portfolio.Portfolio.cash);
  printf "Equity (cash + positions): $%s\n"
    (pretty_money (Engine.equity engine));
  printf "Level: %d\n" (Engine.level engine);
  let positions = Portfolio.all_positions portfolio in
  if List.is_empty positions then
    printf "Positions: none\n"
  else (
    printf "Positions:\n";
    List.iter positions ~f:(fun pos ->
        let dir =
          match pos.Portfolio.direction with
          | Portfolio.Long -> "LONG"
          | Portfolio.Short -> "SHORT"
        in
        printf "  - %s: %s %d share(s) @ $%s avg\n"
          (Ticker.to_string pos.Portfolio.ticker)
          dir
          pos.Portfolio.quantity
          (pretty_money pos.Portfolio.avg_cost)));
  let open_orders = Engine.open_orders engine in
  if List.is_empty open_orders then
    printf "Open orders: none\n"
  else (
    printf "Open orders:\n";
    List.iter open_orders ~f:(fun order ->
        let side =
          match order.Order.type_of_order with
          | Buy -> "BUY"
          | Sell -> "SELL"
        in
        let kind =
          match order.Order.kind with
          | Market -> "market"
          | Limit px -> "limit $" ^ pretty_money px
          | Stop_loss px -> "stop-loss $" ^ pretty_money px
        in
        printf "  - #%d %s %d %s (%s)\n"
          order.Order.id side order.Order.quantity
          (Ticker.to_string order.Order.ticker) kind))

(* Function: validate and convert optional dollars to cents (when user inputs a price) *)
let price_arg_to_cents = function
  | None -> Or_error.return None
  | Some dollars ->
      if Float.(dollars <= 0.) then
        Or_error.errorf "Price must be positive, got %f" dollars
      else
        Or_error.return (Some (Money.of_float_dollars dollars))

(* Function: reject non-positive quantities (when user inputs a quantity) *)
let qty_must_be_positive qty =
  if qty <= 0 then Or_error.errorf "Quantity must be positive, got %d" qty
  else Or_error.return ()

(* Function: check cash before a buy order (when user inputs a price) *)
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
      (pretty_money cost)
      (pretty_money cash)
  else
    return ()

(* Function: block sells when there is no/insufficient long position *)
let ensure_can_sell ~engine ~ticker ~qty =
  let open Or_error.Let_syntax in
  match
    Portfolio.position_for (Engine.portfolio engine) (Ticker.of_string ticker)
  with
  | None ->
      Or_error.errorf
        "Cannot sell: you need an existing position in %s before selling."
        ticker
  | Some pos -> (
      match pos.Portfolio.direction with
      | Portfolio.Long ->
          if qty > pos.quantity then
            Or_error.errorf
              "Cannot sell %d share(s) of %s; you only have %d."
              qty ticker pos.quantity
          else
            Or_error.return ()
      | Portfolio.Short ->
          (* Disallow increasing a short when user has no long shares to sell *)
          Or_error.errorf
            "Cannot sell: %s is already a short position; cover with a buy \
             instead."
            ticker )


(* Function: build order, save state, and report status *)
let place_order ~state ~ticker ~qty ~price_opt ~side =
  let open Or_error.Let_syntax in
  let ticker_sym = Ticker.of_string ticker in
  let%bind () = qty_must_be_positive qty in
  let%bind limit_or_stop = price_arg_to_cents price_opt in
  let%bind engine = load_engine ~state in
  (* Margin check for buys (when user inputs a price) *)
  let%bind () =
    match (side, limit_or_stop) with
    | Order.Buy, Some px ->
        ensure_can_afford_buy ~engine ~ticker ~qty ~price_cents:px
    | Order.Buy, None -> (
        match Map.find (Engine.prices engine) ticker with
        | None ->
            Or_error.errorf "No price available for ticker %s" ticker
        | Some px ->
            ensure_can_afford_buy ~engine ~ticker ~qty ~price_cents:px)
    | Order.Sell, _ ->
        ensure_can_sell ~engine ~ticker ~qty
  in
  let order =
    match (side, limit_or_stop) with
    | _, None ->
        (* Market order *)
        Order.create_market ~ticker:ticker_sym ~type_of_order:side ~quantity:qty ?id:None
    | Order.Buy, Some px ->
        (* Limit buy *)
        Order.create_limit ~ticker:ticker_sym ~type_of_order:side ~quantity:qty
          ~limit_price:px ?id:None
    | Order.Sell, Some px ->
        (* Stop-loss sell: triggers when price <= px (price value) *)
        Order.create_stop_loss ~ticker:ticker_sym ~type_of_order:side ~quantity:qty
          ~stop_price:px ?id:None
  in
  let engine', exec_opt = Engine.submit_order engine ~order in
  let%bind () = save_engine ~state engine' in
  (match exec_opt with
   | Some exec ->
       printf "%s order filled at $%s\n%!"
         (match side with Buy -> "Buy" | Sell -> "Sell")
         (pretty_money exec.Order.fill_price)
   | None ->
       (* For limit buys and stop-loss sells, order may remain open. *)
       printf
         "%s order recorded as an open order.\n%!"
         (match side with Buy -> "Buy" | Sell -> "Sell"));
  print_positions engine';
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
            (pretty_money old_price)
            (pretty_money new_price)
            (if Money.to_int_cents delta >= 0 then "+" else "")
            (pretty_money delta)
            pct;
          None
      | _ -> None)
    |> ignore;
    print_positions final_engine;
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
  print_positions engine;
  print_prices engine;
  return engine

(* Function: copy current state file to a new target (specified by the user)*)
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
  print_positions engine;
  print_prices engine;
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

(* ----- Interactive shell ----- *)

(* Function: print the greeting banner *)
let print_welcome () =
  (* Greeting banner *)
  printf "Welcome to Pixel Portfolio!\n%!"

(* Function: show help text *)
let print_help () =
  printf "Commands:\n";
  printf "  new [easy|medium|hard] [state]   Start a new game (default state=%s).\n"
    default_state_file;
  printf "  save <file> [state]              Save current state to file.\n";
  printf "  portfolio [state]                Show cash, equity, positions, open orders.\n";
  printf "  buy <ticker> <qty> [price] [state]    Market or limit buy.\n";
  printf "  sell <ticker> <qty> [price] [state]   Market or stop/limit sell.\n";
  printf "  cancel <id> [state]             Cancel an open order.\n";
  printf "  simulate [steps] [state]         Run N steps (default 1).\n";
  printf "  help                             Show this help.\n";
  printf "  exit                             Quit the game.\n%!"

(* Function: parse difficulty strings to config enum (easy, medium, hard) *)
let parse_difficulty = Game_config.of_string

(* Function: prompt for yes/no with a default choice *)
let ask_yes_no ~default prompt =
  let suffix = if default then "[Y/n]" else "[y/N]" in
  printf "%s %s %!" prompt suffix;
  match In_channel.input_line In_channel.stdin with
  | None -> default
  | Some line ->
      (match String.lowercase (String.strip line) with
       | "" -> default
       | "y" | "yes" -> true
       | "n" | "no" -> false
       | _ -> default)

(* Function: stdin prompt for difficulty selection *)
let rec prompt_difficulty () =
  printf "Choose difficulty (easy/medium/hard, default=easy): %!";
  match In_channel.input_line In_channel.stdin with
  | None | Some "" -> Difficulty.Easy
  | Some line ->
      let line = String.lowercase (String.strip line) in
      (match parse_difficulty line with
       | Some d -> d
       | None ->
           printf "Unknown difficulty, please try again.\n%!";
           prompt_difficulty ())


(* Function: split words in a line on spaces/tabs *)
let split_input line =
  String.split_on_chars ~on:[' '; '\t'] (String.strip line)
  |> List.filter ~f:(fun s -> not (String.is_empty s))

(* Function: wrap Or_error as Cmdliner Ok/Error for commands *)
let to_cmdliner = function
  | Ok () -> `Ok ()
  | Error err -> `Error (false, Error.to_string_hum err)

(* Cmdliner argument definitions *)
(* Value: optional state file argument with default (default default state file) *)
let state_arg =
  let doc = "State file to read/write." in
  Arg.(value & opt string default_state_file & info [ "state" ] ~docv:"STATE" ~doc)

(* Value: difficulty option argument with enum choices (easy, medium, hard) *)
let mode_arg =
  let doc = "Difficulty: easy, medium, hard." in
  let choices =
    List.map Difficulty.all ~f:(fun d ->
        (Difficulty.to_string d, d))
  in
  Arg.(
    value
    & opt (enum choices) Difficulty.Easy
    & info [ "mode" ] ~docv:"MODE" ~doc
  )

(* Function: positional filename argument constructor *)
let file_arg docv doc =
  Arg.(required & pos 0 (some string) None & info ~doc ~docv [])

(* Value: required ticker symbol positional argument *)
let ticker_arg =
  Arg.(required & pos 0 (some string) None & info ~doc:"Ticker symbol" ~docv:"TICKER" [])

(* Value: required quantity positional argument *)
let qty_arg =
  Arg.(required & pos 1 (some int) None & info ~doc:"Quantity (positive int)" ~docv:"QTY" [])

(* Value: optional price positional argument *)
let price_arg =
  Arg.(value & pos 2 (some float) None & info ~doc:"Limit/stop price (omit for market)" ~docv:"PRICE" [])

(* Value: optional steps flag with default (default 1) (simulation ticks) *)
let steps_arg =
  Arg.(
    value
    & opt int 1
    & info
        ~doc:(Printf.sprintf
                "Number of steps to simulate (1-%d, default 1)" max_sim_steps)
        ~docv:"STEPS"
        [ "steps" ])

(* Value: required order id positional argument *)
let order_id_arg =
  Arg.(
    required
    & pos 0 (some int) None
    & info ~doc:"Order id to cancel (see portfolio open orders)" ~docv:"ID" [])

(* Cmdliner command terms *)
(* Value: Cmdliner command for starting a new game *)
let new_term =
  let doc = "Start a new game with the selected difficulty." in
  let info = Cmd.info "new" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg
         and+ mode = mode_arg in
         new_game ~state ~mode |> Or_error.map ~f:(fun _ -> ()) |> to_cmdliner))
  in
  Cmd.v info term

(* Value: Cmdliner command for loading a save file *)
let load_term =
  let doc = "Load a saved game into the current state file." in
  let info = Cmd.info "load" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg
         and+ file = file_arg "FILE" "Save file to load" in
         load_into ~state ~source:file |> Or_error.map ~f:(fun _ -> ()) |> to_cmdliner))
  in
  Cmd.v info term

(* Value: Cmdliner command for saving to a file *)
let save_term =
  let doc = "Save the current game state to a file." in
  let info = Cmd.info "save" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg
         and+ file = file_arg "FILE" "Destination file" in
         save_as ~state ~target:file |> to_cmdliner))
  in
  Cmd.v info term

(* Value: Cmdliner command to display portfolio/prices *)
let portfolio_term =
  let doc = "Show cash, equity, positions, open orders, and prices." in
  let info = Cmd.info "portfolio" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg in
         load_engine ~state
         |> Or_error.map ~f:(fun eng ->
                print_positions eng;
                print_prices eng)
         |> to_cmdliner))
  in
  Cmd.v info term

(* Value: Cmdliner command to advance simulation *)
let simulate_term =
  let doc = "Advance the market by N steps (default 1)." in
  let info = Cmd.info "simulate" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg
         and+ steps = steps_arg in
         simulate ~state ~steps |> to_cmdliner))
  in
  Cmd.v info term

(* Value: Cmdliner command to place buy orders *)
let buy_term =
  let doc = "Buy shares (market if no price, limit/stop if price provided)." in
  let info = Cmd.info "buy" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg
         and+ ticker = ticker_arg
         and+ qty = qty_arg
         and+ price = price_arg in
         place_order ~state ~ticker ~qty ~price_opt:price ~side:Order.Buy
         |> to_cmdliner))
  in
  Cmd.v info term

(* Value: Cmdliner command to place sell orders *)
let sell_term =
  let doc = "Sell shares (market if no price, limit/stop if price provided)." in
  let info = Cmd.info "sell" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg
         and+ ticker = ticker_arg
         and+ qty = qty_arg
         and+ price = price_arg in
         place_order ~state ~ticker ~qty ~price_opt:price ~side:Order.Sell
         |> to_cmdliner))
  in
  Cmd.v info term

(* Value: Cmdliner command to cancel open orders *)
let cancel_term =
  let doc = "Cancel an open order by id." in
  let info = Cmd.info "cancel" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg
         and+ id = order_id_arg in
         cancel_order_cmd ~state ~order_id:id |> to_cmdliner))
  in
  Cmd.v info term

(* Value: Cmdliner command group for all subcommands (to dispatch right command) *)
let default_cmd =
  let doc = "Pixel Portfolio stochastic market simulator (interactive + CLI)." in
  let info = Cmd.info "pixel_portfolio" ~doc in
  Cmd.group info
    [ new_term;
      load_term;
      save_term;
      portfolio_term;
      simulate_term;
      buy_term;
      sell_term;
      cancel_term ]

(* Function: forward a parsed line into Cmdliner evaluation (takes input and dispatches right command) *)
let run_cmd tokens =
  let argv = Array.of_list ("pixel_portfolio" :: tokens) in
  match Cmd.eval_value ~catch:false ~argv default_cmd with
  | Ok (`Ok ()) -> true (* command executed successfully *)
  | Ok `Help -> true (* help command requested *)
  | Ok `Version -> true (* version command requested *)
  | Error _ -> false (* command execution failed *)

(* Function: main interactive loop reading and dispatching commands *)
let rec repl () =
  printf "> %!";
  match In_channel.input_line In_channel.stdin with
  | None -> ()
  | Some line ->
      let tokens = split_input line in
      (match tokens with
       | [] -> repl ()
       | ("exit" :: _ | "quit" :: _) -> ()
       | ["help"] ->
           print_help ();
           repl ()
       | _ ->
           ignore (run_cmd tokens);
           repl ())

(* Function: choose new vs load, then launch *)
let start_initial_engine () =
  print_welcome ();
  let has_save = Stdlib.Sys.file_exists default_state_file in
  let start_new =
    if has_save then
      ask_yes_no ~default:false
        "Start new game?"
    else (
      printf "No saved game found.\n%!";
      true)
  in
  if start_new then (
    printf "Starting a new game (this will overwrite %s).\n%!"
      default_state_file;
    let mode = prompt_difficulty () in
    Or_error.ok_exn (new_game ~state:default_state_file ~mode))
  else (
    match load_engine ~state:default_state_file with
    | Ok eng ->
        printf "Loaded existing game from %s\n%!" default_state_file;
        print_positions eng;
        print_prices eng;
        eng
    | Error err ->
        eprintf "Failed to load existing game (%s). Starting new instead.\n%!"
          (Error.to_string_hum err);
        let mode = prompt_difficulty () in
        Or_error.ok_exn (new_game ~state:default_state_file ~mode))

(* initialize game *)
let () =
  ignore (start_initial_engine ());
  printf "\n";
  print_help ();
  printf "\n";
  repl ()

