open Core

(* Value: cap for absolute noise magnitude *)
let max_abs_noise = 0.2

(* Value: width of uniform noise interval *)
let noise_range = 2.0 *. max_abs_noise

(* Value: dedicated RNG for price noise *)
let random_state = Random.State.make [| 0x5eed_cafe |]

(* Function: draw uniform noise in [-max_abs_noise, max_abs_noise] *)
let sample () =
  (* Sample uniformly in [-max_abs_noise, max_abs_noise] *)
  Random.State.float random_state noise_range -. max_abs_noise

(* Function: fill missing tickers with sampled noise *)
let ensure_noise_map ~universe ~existing =
  Map.fold universe ~init:existing ~f:(fun ~key ~data:_ acc ->
      if Map.mem acc key then acc
      else Map.set acc ~key ~data:(sample ()))
