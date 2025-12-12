open Core
open Cmdliner
open Cmdliner.Term.Syntax
open Pixel_portfolio_lib

(* ----- Game configuration ----- *)

type difficulty =
  | Easy
  | Medium
  | Hard

(* Map difficulty to starting cash *)
let difficulty_to_cash = function
  | Easy -> Money.float_dollars_to_cents 100_000.0
  | Medium -> Money.float_dollars_to_cents 25_000.0
  | Hard -> Money.float_dollars_to_cents 5_000.0

let default_state_file = "game.sexp"

let max_sim_steps = 160

(* Dollar helper *)
let cents_of_dollars dollars = Money.float_dollars_to_cents dollars

(* Build a GBM asset *)
let mk_gbm_asset ticker ~price ~mu ~sigma =
  let process = Model.GBM (Gbm.create ~mu ~sigma ~dt:1.0) in
  { Model.ticker; process; initial_price = cents_of_dollars price }

(* Build an OU asset *)
let mk_ou_asset ticker ~price ~kappa ~theta ~sigma =
  let process = Model.OU (Ou.create ~kappa ~theta ~sigma ~dt:1.0) in
  { Model.ticker; process; initial_price = cents_of_dollars price }

let base_universe =
  (* Default universe of tickers with GBM/OU processes *)
  let trending =
    [
      mk_gbm_asset "AAPL" ~price:195.0 ~mu:0.12 ~sigma:0.25;
      mk_gbm_asset "AMZN" ~price:140.0 ~mu:0.11 ~sigma:0.30;
      mk_gbm_asset "GOOG" ~price:125.0 ~mu:0.10 ~sigma:0.22;
      mk_gbm_asset "NVDA" ~price:450.0 ~mu:0.18 ~sigma:0.55;
      mk_gbm_asset "PLTR" ~price:18.5 ~mu:0.20 ~sigma:0.40;
    ]
  and penny =
    [
      mk_ou_asset "DKSC" ~price:1.20 ~kappa:1.6 ~theta:1.10 ~sigma:0.35;
      mk_ou_asset "RNWF" ~price:0.80 ~kappa:1.4 ~theta:0.75 ~sigma:0.40;
      mk_ou_asset "BCEKF" ~price:1.90 ~kappa:1.2 ~theta:1.85 ~sigma:0.25;
      mk_ou_asset "SVRSF" ~price:1.05 ~kappa:1.3 ~theta:1.0 ~sigma:0.30;
      mk_ou_asset "T1E" ~price:2.50 ~kappa:0.9 ~theta:2.4 ~sigma:0.18;
    ]
  in
  List.append trending penny
  |> List.fold ~init:Model.empty_universe ~f:Model.add_asset

let create_engine difficulty =
  (* Build engine config from difficulty and universe *)
  let config =
    {
      Engine.universe = base_universe;
      initial_prices = String.Map.empty;
      initial_cash = difficulty_to_cash difficulty;
    }
  in
  Engine.create config

(* ----- IO helpers ----- *)

(* Copy a file *)
let copy_file ~src ~dst =
  Or_error.try_with (fun () ->
      let data = In_channel.read_all src in
      Out_channel.write_all dst ~data)

(* Backup a file if it exists *)
let backup_if_exists path =
  if Stdlib.Sys.file_exists path then
    let backup = path ^ ".bak" in
    copy_file ~src:path ~dst:backup
    |> Or_error.map ~f:(fun () ->
           printf "Existing state saved to %s\n%!" backup)
  else
    Or_error.return ()

let save_engine ~state engine =
  match Save_game_state.save_sexp ~filename:state engine with
  | Save_game_state.Pass _ -> Or_error.return ()
  | Fail msg -> Or_error.error_string msg

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

let ensure_source_exists path =
  if Stdlib.Sys.file_exists path then Or_error.return ()
  else Or_error.errorf "File %s does not exist." path

let pretty_money amount = Money.make_it_look_nice amount

let initial_price_map (engine : Engine.t) =
  let cfg = Engine.config engine in
  if Map.is_empty cfg.initial_prices then
    Model.initial_prices cfg.universe
  else
    cfg.initial_prices

let print_prices engine =
  (* Show current prices with deltas vs initial *)
  let current = Engine.prices engine in
  let reference = initial_price_map engine in
  printf "\nCurrent prices (t=%d):\n" (Engine.time_index engine);
  Map.iteri current ~f:(fun ~key:ticker ~data:price ->
      let ref_price =
        Map.find reference ticker
        |> Option.value ~default:price
      in
      let delta = price - ref_price in
      let pct =
        if ref_price = 0 then 0.0
        else
          let d = Money.cents_to_float_dollars delta in
          let r = Money.cents_to_float_dollars ref_price in
          (d /. r) *. 100.0
      in
      printf "  %s: $%s (%s%s, %.2f%%)\n"
        ticker
        (pretty_money price)
        (if delta >= 0 then "+" else "")
        (pretty_money delta)
        pct);
  printf "%!"

let rec advance_ticks engine steps =
  (* Run N ticks, generating noise internally *)
  if steps <= 0 then engine
  else
    let next_engine = Engine.tick engine ~noise:String.Map.empty in
    advance_ticks next_engine (steps - 1)

let print_positions engine =
  (* Show cash, equity, positions, open orders *)
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

let price_arg_to_cents = function
  | None -> Or_error.return None
  | Some dollars ->
      if Float.(dollars <= 0.) then
        Or_error.errorf "Price must be positive, got %f" dollars
      else
        Or_error.return (Some (Money.float_dollars_to_cents dollars))

(* Quantity must be positive *)
let qty_must_be_positive qty =
  if qty <= 0 then Or_error.errorf "Quantity must be positive, got %d" qty
  else Or_error.return ()

let ensure_can_afford_buy ~engine ~ticker ~qty ~price_cents =
  let open Or_error.Let_syntax in
  let open Money in
  let cost = multiply price_cents qty in
  let cash = (Engine.portfolio engine).Portfolio.cash in
  if cost > cash then
    Or_error.errorf
      "Insufficient cash to buy %d share(s) of %s: need $%s but only have $%s"
      qty
      ticker
      (pretty_money cost)
      (pretty_money cash)
  else
    return ()

let place_order ~state ~ticker ~qty ~price_opt ~side =
  (* Create market/limit/stop-loss order, persist state, and print status *)
  let open Or_error.Let_syntax in
  let%bind () = qty_must_be_positive qty in
  let%bind limit_or_stop = price_arg_to_cents price_opt in
  let%bind engine = load_engine ~state in
  (* Margin check for buys *)
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
        Or_error.return ()
  in
  let order =
    match (side, limit_or_stop) with
    | _, None ->
        (* Market order *)
        Order.create_market ~ticker ~type_of_order:side ~quantity:qty ?id:None
    | Order.Buy, Some px ->
        (* Limit buy *)
        Order.create_limit ~ticker ~type_of_order:side ~quantity:qty
          ~limit_price:px ?id:None
    | Order.Sell, Some px ->
        (* Stop-loss sell: triggers when price <= px *)
        Order.create_stop_loss ~ticker ~type_of_order:side ~quantity:qty
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

let simulate ~state ~steps =
  (* Advance simulation N steps, save, and show deltas *)
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
          let delta = new_price - old_price in
          let pct =
            if old_price = 0 then 0.0
            else
              let d = Money.cents_to_float_dollars delta in
              let o = Money.cents_to_float_dollars old_price in
              (d /. o) *. 100.0
          in
          printf "  %s: $%s -> $%s (%s%s, %.2f%%)\n"
            ticker
            (pretty_money old_price)
            (pretty_money new_price)
            (if delta >= 0 then "+" else "")
            (pretty_money delta)
            pct;
          None
      | _ -> None)
    |> ignore;
    print_positions final_engine;
    return ()

let new_game ~state ~mode =
  (* Start a new game, backing up any existing state file *)
  let open Or_error.Let_syntax in
  let%bind () = backup_if_exists state in
  let engine = create_engine mode in
  let%bind () = save_engine ~state engine in
  printf "Started new %s game at %s.\n"
    (match mode with Easy -> "easy" | Medium -> "medium" | Hard -> "hard")
    state;
  print_positions engine;
  print_prices engine;
  return engine

let save_as ~state ~target =
  (* Copy current state file to a new target *)
  let open Or_error.Let_syntax in
  let%bind () = ensure_source_exists state in
  let%bind engine = load_engine ~state in
  let%bind () = save_engine ~state:target engine in
  printf "Saved current game to %s\n%!" target;
  return ()

let load_into ~state ~source =
  (* Load from source into state, backing up the current state *)
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

let print_welcome () =
  (* Greeting banner *)
  printf "Welcome to Pixel Portfolio!\n%!"

let print_help () =
  (* REPL help; CLI commands still parsed by Cmdliner *)
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

let parse_difficulty = function
  | "easy" -> Some Easy
  | "medium" -> Some Medium
  | "hard" -> Some Hard
  | _ -> None

let ask_yes_no ~default prompt =
  (* Simple yes/no stdin prompt with default *)
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

let rec prompt_difficulty () =
  (* Simple stdin prompt for difficulty selection *)
  printf "Choose difficulty (easy/medium/hard, default=easy): %!";
  match In_channel.input_line In_channel.stdin with
  | None | Some "" -> Easy
  | Some line ->
      let line = String.lowercase (String.strip line) in
      (match parse_difficulty line with
       | Some d -> d
       | None ->
           printf "Unknown difficulty, please try again.\n%!";
           prompt_difficulty ())

let split_input line =
  (* Tokenize a line on spaces/tabs *)
  String.split_on_chars ~on:[' '; '\t'] (String.strip line)
  |> List.filter ~f:(fun s -> not (String.is_empty s))

let to_cmdliner = function
  (* Convert Or_error to Cmdliner result shape *)
  | Ok () -> `Ok ()
  | Error err -> `Error (false, Error.to_string_hum err)

(* Cmdliner argument definitions *)
let state_arg =
  let doc = "State file to read/write." in
  Arg.(value & opt string default_state_file & info [ "state" ] ~docv:"STATE" ~doc)

let mode_arg =
  let doc = "Difficulty: easy, medium, hard." in
  Arg.(value & opt (enum [ ("easy", Easy); ("medium", Medium); ("hard", Hard) ]) Easy
       & info [ "mode" ] ~docv:"MODE" ~doc)

let file_arg docv doc =
  Arg.(required & pos 0 (some string) None & info ~doc ~docv [])

let ticker_arg =
  Arg.(required & pos 0 (some string) None & info ~doc:"Ticker symbol" ~docv:"TICKER" [])

let qty_arg =
  Arg.(required & pos 1 (some int) None & info ~doc:"Quantity (positive int)" ~docv:"QTY" [])

let price_arg =
  Arg.(value & pos 2 (some float) None & info ~doc:"Limit/stop price (omit for market)" ~docv:"PRICE" [])

let steps_arg =
  Arg.(
    value
    & opt int 1
    & info
        ~doc:(Printf.sprintf
                "Number of steps to simulate (1-%d, default 1)" max_sim_steps)
        ~docv:"STEPS"
        [ "steps" ])

let order_id_arg =
  Arg.(
    required
    & pos 0 (some int) None
    & info ~doc:"Order id to cancel (see portfolio open orders)" ~docv:"ID" [])

(* Cmdliner command terms *)
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

let run_cmd tokens =
  (* Forward a parsed line into Cmdliner command evaluation *)
  let argv = Array.of_list ("pixel_portfolio" :: tokens) in
  match Cmd.eval_value ~catch:false ~argv default_cmd with
  | Ok (`Ok ()) -> true
  | Ok `Help -> true
  | Ok `Version -> true
  | Error _ -> false

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

let () =
  ignore (start_initial_engine ());
  printf "\n";
  print_help ();
  printf "\n";
  repl ()

