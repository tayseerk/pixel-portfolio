open Core

let max_abs_noise = 0.2
let noise_range = 2.0 *. max_abs_noise

(* Dedicated RNG for price noise. *)
let random_state = Random.State.make [| 0x5eed_cafe |]

let sample () =
  (* Sample uniformly in [-max_abs_noise, max_abs_noise]. *)
  Random.State.float random_state noise_range -. max_abs_noise

let ensure_noise_map ~universe ~existing =
  Map.fold universe ~init:existing ~f:(fun ~key ~data:_ acc ->
      if Map.mem acc key then acc
      else Map.set acc ~key ~data:(sample ()))
