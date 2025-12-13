open Core
open Pixel_portfolio_lib

module Difficulty = Game_config
module Runtime = Game_runtime
module View = Game_view

(* This must be in bin because its built as an executable not built as a library*)
(* Value: default state file for save and load files *)
let default_state_file = Game_config.default_state_file

(* ----- Interactive shell helpers ----- *)

let print_welcome () =
  printf "Welcome to Pixel Portfolio!\n%!"

let print_help () =
  printf "Commands:\n";
  printf "  new [easy|medium|hard] [state]   Start a new game (default state=%s).\n"
    default_state_file;
  printf "  save <file> [state]              Save current state to file.\n";
  printf "  portfolio [state]                Show cash, equity, positions, open orders.\n";
  printf "  buy <ticker> <qty> [state]       Market buy.\n";
  printf "  sell <ticker> <qty> [price] [state]   Market or stop/limit sell.\n";
  printf "  cancel <id> [state]             Cancel an open order.\n";
  printf "  simulate [steps] [state]         Run N steps (default 1).\n";
  printf "  help                             Show this help.\n";
  printf "  exit                             Quit the game.\n%!"

let parse_difficulty = Game_config.of_string

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

let split_input line =
  String.split_on_chars ~on:[' '; '\t'] (String.strip line)
  |> List.filter ~f:(fun s -> not (String.is_empty s))

(* Choose new vs load, then launch *)
let start_initial_engine () =
  print_welcome ();
  let has_save = Stdlib.Sys.file_exists default_state_file in
  let start_new =
    if has_save then
      ask_yes_no ~default:false "Start new game?"
    else (
      printf "No saved game found.\n%!";
      true)
  in
  if start_new then (
    printf "Starting a new game (this will overwrite %s).\n%!"
      default_state_file;
    let mode = prompt_difficulty () in
    Or_error.ok_exn (Runtime.new_game ~state:default_state_file ~mode))
  else (
    match Runtime.load_engine ~state:default_state_file with
    | Ok eng ->
        printf "Loaded existing game from %s\n%!" default_state_file;
        View.print_positions eng;
        View.print_prices eng;
        eng
    | Error err ->
        eprintf "Failed to load existing game (%s). Starting new instead.\n%!"
          (Error.to_string_hum err);
        let mode = prompt_difficulty () in
        Or_error.ok_exn (Runtime.new_game ~state:default_state_file ~mode)
  )

let rec repl ~run_cmd =
  printf "> %!";
  match In_channel.input_line In_channel.stdin with
  | None -> ()
  | Some line ->
      let tokens = split_input line in
      (match tokens with
       | [] -> repl ~run_cmd
       | ("exit" :: _ | "quit" :: _) -> ()
       | ["help"] ->
           print_help ();
           repl ~run_cmd
       | _ ->
           ignore (run_cmd tokens);
           repl ~run_cmd)
