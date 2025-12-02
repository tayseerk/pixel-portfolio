open Core
open Cmdliner
open Pixel_portfolio_lib

(* ---------- Common arguments ---------- *)

let ticker_arg =
  let doc = "Stock ticker symbol, e.g. AAPL, GOOGL, TSLA." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"TICKER" ~doc)

let qty_arg =
  let doc = "Quantity of shares to buy or sell." in
  Arg.(required & opt (some int) None & info ["q"; "qty"] ~docv:"QTY" ~doc)

let price_arg =
  let doc = "Limit or reference price in dollars, e.g. 100.25." in
  Arg.(value & opt (some string) None & info ["p"; "price"] ~docv:"PRICE" ~doc)

let steps_arg =
  let doc = "Number of simulation steps / ticks to run." in
  Arg.(value & opt int 10 & info ["n"; "steps"] ~docv:"N" ~doc)

let file_arg =
  let doc = "Path to a save file for loading/saving game state." in
  Arg.(required & opt (some string) None & info ["f"; "file"] ~docv:"FILE" ~doc)

(* ---------- Simulation helpers ---------- *)

let cents_of_dollars dollars =
  Money.float_dollars_to_cents dollars

let mk_gbm_asset ticker ~price ~mu ~sigma =
  let process = Model.GBM (Gbm.create ~mu ~sigma ~dt:1.0) in
  {
    Model.ticker;
    process;
    initial_price = cents_of_dollars price;
  }

let mk_ou_asset ticker ~price ~kappa ~theta ~sigma =
  let process = Model.OU (Ou.create ~kappa ~theta ~sigma ~dt:1.0) in
  {
    Model.ticker;
    process;
    initial_price = cents_of_dollars price;
  }

let default_universe =
  [
    mk_gbm_asset "PLTR" ~price:18.50 ~mu:0.18 ~sigma:0.38;
    mk_gbm_asset "TSLA" ~price:220.00 ~mu:0.08 ~sigma:0.45;
    mk_ou_asset "BND" ~price:75.00 ~kappa:1.1 ~theta:74.5 ~sigma:0.15;
  ]
  |> List.fold ~init:Model.empty_universe ~f:Model.add_asset

let default_engine () =
  let config : Engine.config =
    {
      universe = default_universe;
      initial_prices = String.Map.empty;
      initial_cash = cents_of_dollars 50_000.0;
    }
  in
  Engine.create config

let empty_noise : float String.Map.t = String.Map.empty

let rec advance_ticks engine steps =
  if steps <= 0 then engine
  else
    let next_engine = Engine.tick engine ~noise:empty_noise in
    advance_ticks next_engine (steps - 1)

let pretty_money amount =
  Money.make_it_look_nice amount

let print_prices prices =
  printf "Prices:\n";
  Map.iteri prices ~f:(fun ~key:ticker ~data:price ->
      printf "  - %s: $%s\n" ticker (pretty_money price));
  printf "%!"

let report_simulation engine steps =
  printf "Simulated %d tick(s). Current time index: %d\n"
    steps (Engine.time_index engine);
  print_prices (Engine.prices engine);
  printf "Equity: $%s\n%!" (pretty_money (Engine.equity engine))

(* ---------- Command handlers (placeholders only) ---------- *)

let buy_handler ticker qty price_opt =
  let price_str =
    match price_opt with
    | None -> "market price (no explicit price provided)"
    | Some p -> Printf.sprintf "$%s" p
  in
  printf "Not implemented yet.\n";
  printf "Expected behavior:\n";
  printf "  - Create a BUY order for %d share(s) of %s at %s.\n" qty ticker price_str;
  printf "  - Send it to the trading engine (Engine.submit_order).\n";
  printf "  - Update the portfolio cash/positions if the order is filled.\n";
  printf "  - Print the new cash balance and position summary.\n%!"

let sell_handler ticker qty price_opt =
  let price_str =
    match price_opt with
    | None -> "market price (no explicit price provided)"
    | Some p -> Printf.sprintf "$%s" p
  in
  printf "Not implemented yet.\n";
  printf "Expected behavior:\n";
  printf "  - Create a SELL order for %d share(s) of %s at %s.\n" qty ticker price_str;
  printf "  - Send it to the trading engine (Engine.submit_order).\n";
  printf "  - Update the portfolio by reducing the position and adjusting cash.\n";
  printf "  - Print realized/unrealized P&L and current positions.\n%!"

let portfolio_handler () =
  printf "Not implemented yet.\n";
  printf "Expected behavior:\n";
  printf "  - Query the current portfolio from the engine.\n";
  printf "  - Print cash balance, each open position (ticker, qty, avg cost).\n";
  printf "  - Print total equity and maybe unrealized P&L.\n%!"

let tick_handler () =
  let engine = default_engine () in
  let final_engine = advance_ticks engine 1 in
  report_simulation final_engine 1

let simulate_handler steps =
  if steps <= 0 then (
    printf "Number of steps must be positive. Received %d.\n%!" steps
  ) else
    let engine = default_engine () in
    let final_engine = advance_ticks engine steps in
    report_simulation final_engine steps

let save_handler file =
  printf "Not implemented yet.\n";
  printf "Expected behavior:\n";
  printf "  - Serialize the current Engine.t using Save_game_state.save_sexp or save_json.\n";
  printf "  - Write it to the file: %s.\n" file;
  printf "  - Print a confirmation or error message.\n%!"

let load_handler file =
  printf "Not implemented yet.\n";
  printf "Expected behavior:\n";
  printf "  - Read the engine state from %s using Save_game_state.load_sexp or load_json.\n" file;
  printf "  - Replace the current Engine.t in memory with the loaded one.\n";
  printf "  - Print a brief summary of the restored portfolio/prices.\n%!"

(* ---------- Cmdliner term & command definitions ---------- *)

let buy_term =
  let doc = "Place a BUY order (placeholder; does not change state yet)." in
  let man = [
    `S "EXAMPLES";
    `P "Buy 10 shares of AAPL at market price:";
    `Pre "  pixel_portfolio buy AAPL --qty 10";
    `P "Buy 5 shares of TSLA with an explicit limit price:";
    `Pre "  pixel_portfolio buy TSLA --qty 5 --price 200.50";
  ] in
  let info = Cmd.info "buy" ~doc ~man in
  let term = Term.(const buy_handler $ ticker_arg $ qty_arg $ price_arg) in
  Cmd.v info term

let sell_term =
  let doc = "Place a SELL order (placeholder; does not change state yet)." in
  let man = [
    `S "EXAMPLES";
    `P "Sell 3 shares of AAPL at market price:";
    `Pre "  pixel_portfolio sell AAPL --qty 3";
  ] in
  let info = Cmd.info "sell" ~doc ~man in
  let term = Term.(const sell_handler $ ticker_arg $ qty_arg $ price_arg) in
  Cmd.v info term

let portfolio_term =
  let doc = "Show current portfolio (placeholder output)." in
  let man = [
    `S "EXAMPLES";
    `P "Display portfolio summary:";
    `Pre "  pixel_portfolio portfolio";
  ] in
  let info = Cmd.info "portfolio" ~doc ~man in
  let term = Term.(const portfolio_handler $ const ()) in
  Cmd.v info term

let tick_term =
  let doc = "Advance the simulation by one tick and show updated prices." in
  let man = [
    `S "EXAMPLES";
    `P "Advance by one tick and view prices:";
    `Pre "  pixel_portfolio tick";
  ] in
  let info = Cmd.info "tick" ~doc ~man in
  let term = Term.(const tick_handler $ const ()) in
  Cmd.v info term

let simulate_term =
  let doc = "Run several ticks of the simulation and print final prices/equity." in
  let man = [
    `S "EXAMPLES";
    `P "Simulate 20 steps:";
    `Pre "  pixel_portfolio simulate --steps 20";
  ] in
  let info = Cmd.info "simulate" ~doc ~man in
  let term = Term.(const simulate_handler $ steps_arg) in
  Cmd.v info term

let save_term =
  let doc = "Save game state to a file (placeholder)." in
  let man = [
    `S "EXAMPLES";
    `P "Save to game.sexp:";
    `Pre "  pixel_portfolio save --file game.sexp";
  ] in
  let info = Cmd.info "save" ~doc ~man in
  let term = Term.(const save_handler $ file_arg) in
  Cmd.v info term

let load_term =
  let doc = "Load game state from a file (placeholder)." in
  let man = [
    `S "EXAMPLES";
    `P "Load from game.sexp:";
    `Pre "  pixel_portfolio load --file game.sexp";
  ] in
  let info = Cmd.info "load" ~doc ~man in
  let term = Term.(const load_handler $ file_arg) in
  Cmd.v info term

(* ---------- Top-level command group ---------- *)

let default_cmd =
  let doc = "A small stock trading simulation game (placeholder CLI)." in
  let man = [
    `S "DESCRIPTION";
    `P "This command-line interface exposes subcommands like \
        $(b,buy), $(b,sell), $(b,portfolio), $(b,tick), and $(b,simulate).";
    `P "At this stage, all commands only print what they would do \
        once the engine and models are fully implemented.";
  ] in
  let info = Cmd.info "pixel_portfolio" ~version:"0.1" ~doc ~man in
  Cmd.group info
    [ buy_term;
      sell_term;
      portfolio_term;
      tick_term;
      simulate_term;
      save_term;
      load_term;
    ]

let () =
  exit (Cmd.eval default_cmd)