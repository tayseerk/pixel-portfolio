open Core
open Cmdliner

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
  printf "Not implemented yet.\n";
  printf "Expected behavior:\n";
  printf "  - Advance the simulation by one 'tick' using Engine.tick.\n";
  printf "  - Update prices with the GBM/OU models.\n";
  printf "  - Print the new time index and updated prices.\n%!"

let simulate_handler steps =
  printf "Not implemented yet.\n";
  printf "Expected behavior:\n";
  printf "  - Create an initial Engine.t with a small universe of assets.\n";
  printf "  - Run %d successive ticks (Engine.tick in a loop).\n" steps;
  printf "  - At the end, print final prices and portfolio equity.\n";
  printf "  - Optionally log a small price path for one ticker.\n%!"

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
  let doc = "Advance the simulation by one tick (placeholder)." in
  let man = [
    `S "EXAMPLES";
    `P "Advance by one tick and show what would happen:";
    `Pre "  pixel_portfolio tick";
  ] in
  let info = Cmd.info "tick" ~doc ~man in
  let term = Term.(const tick_handler $ const ()) in
  Cmd.v info term

let simulate_term =
  let doc = "Run several ticks of the simulation (placeholder)." in
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