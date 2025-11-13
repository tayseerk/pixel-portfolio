open Core 
open Cmdliner

(* Demo of Cmdliner library for command-line argument parsing *)

let demo_command stock_name shares price_opt =
  printf "=== Cmdliner Demo ===\n\n";
  printf "Stock: %s\n" stock_name;
  printf "Shares: %d\n" shares;
  (match price_opt with
   | Some p -> printf "Limit Price: $%.2f\n" p
   | None -> printf "Market Order (no price limit)\n");
  printf "\nCmdliner demo completed successfully!\n"

(* Define command-line arguments *)
let stock_arg =
  let doc = "Stock ticker symbol (e.g., AAPL, GOOGL)" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"STOCK" ~doc)

let shares_arg =
  let doc = "Number of shares to trade" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"SHARES" ~doc)

let price_opt =
  let doc = "Limit price for the order (optional)" in
  Arg.(value & opt (some float) None & info ["p"; "price"] ~docv:"PRICE" ~doc)

let cmd =
  let doc = "Demo of command-line argument parsing for stock trading" in
  let info = Cmd.info "cli_demo" ~doc in
  Cmd.v info Term.(const demo_command $ stock_arg $ shares_arg $ price_opt)

let () = Cmd.eval cmd |> exit