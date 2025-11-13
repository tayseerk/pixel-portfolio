open Core

(* Demo of Owl library for random number generation *)

(* Helper to run something n times *)
let rec repeat n f =
  if n <= 0 then ()
  else begin
    f ();
    repeat (n - 1) f
  end

let () =
  printf "=== Owl Library Demo ===\n\n";

  (* Generate random samples from normal distribution *)
  printf "1. Normal Distribution (mean=0, std=1):\n";
  repeat 5 (fun () ->
    let sample = Owl.Stats.gaussian_rvs ~mu:0. ~sigma:1. in
    printf "  %.4f\n" sample
  );
  printf "\n";

  (* Generate samples for stock-like returns *)
  printf "2. Normal Distribution (mean=0.05, std=0.2) - like daily stock returns:\n";
  repeat 5 (fun () ->
    let return = Owl.Stats.gaussian_rvs ~mu:0.05 ~sigma:0.2 in
    printf "  %.4f (%.2f%%)\n" return (return *. 100.)
  );
  printf "\n";

  (* Demonstrate correlation matrix *)
  printf "3. Creating a simple correlation matrix:\n";
  let corr_matrix = Owl.Mat.of_arrays [|
    [| 1.0; 0.7; 0.3 |];
    [| 0.7; 1.0; 0.5 |];
    [| 0.3; 0.5; 1.0 |]
  |] in
  printf "  Correlation matrix shape: %d x %d\n"
    (Owl.Mat.row_num corr_matrix)
    (Owl.Mat.col_num corr_matrix);
  Owl.Mat.print corr_matrix;
  printf "\n";

  (* Demonstrate basic array statistics *)
  printf "4. Basic array statistics:\n";
  let data = [| 10.; 20.; 30.; 40.; 50. |] in
  let mean = Owl.Stats.mean data in
  let std = Owl.Stats.std data in
  printf "  Data: [10, 20, 30, 40, 50]\n";
  printf "  Mean: %.2f\n" mean;
  printf "  Std:  %.2f\n" std;
  printf "\n";

  (* Demonstrate simulating a price path *)
  printf "5. Simulating 10 price movements:\n";
  let initial_price = 100.0 in
  let drift = 0.001 in
  let volatility = 0.02 in
  let rec simulate_path price steps =
    if steps = 0 then ()
    else begin
      let shock = Owl.Stats.gaussian_rvs ~mu:0. ~sigma:1. in
      let return = drift +. (volatility *. shock) in
      let new_price = price *. (1. +. return) in
      printf "  Step %d: $%.2f (return: %.2f%%)\n" 
        (11 - steps) new_price (return *. 100.);
      simulate_path new_price (steps - 1)
    end
  in
  simulate_path initial_price 10;
  printf "\n";

  printf "Owl demo completed successfully!\n"