[@@@warning "-16"]
open Core

type t = {
  kappa : float;
  theta : float;
  sigma : float;
  dt : float;
}
[@@deriving sexp]

let create ~kappa ~theta ~sigma ?dt =
  let dt = Option.value dt ~default:1.0 in
  { kappa; theta; sigma; dt }


let min_state = 1e-6

let ensure_positive state =
  (* keep the simulated state strictly positive to avoid negative prices *)
  Float.max state min_state

let step t ~state ~noise =
  let mean_revert = t.kappa *. (t.theta -. state) *. t.dt in
  let diffusion = t.sigma *. Float.sqrt t.dt *. noise in
  let next_state = state +. mean_revert +. diffusion in
  ensure_positive next_state

let simulate_path model ~initial_state ~noises =
  let len = Array.length noises in
  let path = Array.create ~len initial_state in
  let current = ref initial_state in
  for i = 0 to len - 1 do
    let next = step model ~state:!current ~noise:noises.(i) in
    path.(i) <- next;
    current := next
  done;
  path


