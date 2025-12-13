open Core
open Cmdliner
open Cmdliner.Term.Syntax
open Pixel_portfolio_lib

module Difficulty = Game_config
module Runtime = Game_runtime
module View = Game_view
module Shell = Game_shell

(* ----- Game configuration ----- *)

let default_state_file = Game_config.default_state_file

(* Local alias so our help text matches the runtime constraint *)
let max_sim_steps = Runtime.max_sim_steps

(* Function: wrap Or_error as Cmdliner Ok/Error for commands *)
let to_cmdliner = function
  | Ok () -> `Ok ()
  | Error err -> `Error (false, Error.to_string_hum err)

(* ----- Cmdliner argument definitions ----- *)

let state_arg =
  let doc = "State file to read/write." in
  Arg.(value & opt string default_state_file & info [ "state" ] ~docv:"STATE" ~doc)

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

(* ----- Cmdliner command terms ----- *)

let new_term =
  let doc = "Start a new game with the selected difficulty." in
  let info = Cmd.info "new" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg
         and+ mode = mode_arg in
         Runtime.new_game ~state ~mode
         |> Or_error.map ~f:(fun _ -> ())
         |> to_cmdliner))
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
         Runtime.load_into ~state ~source:file
         |> Or_error.map ~f:(fun _ -> ())
         |> to_cmdliner))
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
         Runtime.save_as ~state ~target:file |> to_cmdliner))
  in
  Cmd.v info term

let portfolio_term =
  let doc = "Show cash, equity, positions, open orders, and prices." in
  let info = Cmd.info "portfolio" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg in
         Runtime.load_engine ~state
         |> Or_error.map ~f:(fun eng ->
                View.print_positions eng;
                View.print_prices eng)
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
         Runtime.simulate ~state ~steps |> to_cmdliner))
  in
  Cmd.v info term

let buy_term =
  let doc = "Buy shares at the current market price." in
  let info = Cmd.info "buy" ~doc in
  let term =
    Term.(
      ret
        (let+ state = state_arg
         and+ ticker = ticker_arg
         and+ qty = qty_arg in
         Runtime.place_order ~state ~ticker ~qty ~price_opt:None ~side:Order.Buy
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
         Runtime.place_order ~state ~ticker ~qty ~price_opt:price ~side:Order.Sell
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
         Runtime.cancel_order_cmd ~state ~order_id:id |> to_cmdliner))
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

(* ----- REPL wiring ----- *)

let run_cmd tokens =
  let argv = Array.of_list ("pixel_portfolio" :: tokens) in
  match Cmd.eval_value ~catch:false ~argv default_cmd with
  | Ok (`Ok ()) -> true
  | Ok `Help -> true
  | Ok `Version -> true
  | Error _ -> false

(* initialize game *)
let () =
  ignore (Shell.start_initial_engine ());
  printf "\n";
  Shell.print_help ();
  printf "\n";
  Shell.repl ~run_cmd



