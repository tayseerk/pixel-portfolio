[@@@warning "-16"]
open Core

type t = {
  mu : float;
  sigma : float;
  dt : float;
}
[@@deriving sexp]

let create ~mu ~sigma ?dt =
  let dt = Option.value dt ~default:1.0 in
  { mu; sigma; dt }


let min_price = 1e-6

let ensure_positive price =
  (* keep the simulated price strictly positive to avoid downstream issues *)
  Float.max price min_price

let step t ~price ~noise =
  let drift = (t.mu -. 0.5 *. Float.square t.sigma) *. t.dt in
  let diffusion = t.sigma *. Float.sqrt t.dt *. noise in
  (* cap log-return to keep moves modest (~8% max up/down per tick) *)
  let max_log_step = 0.08 in
  let log_return =
    drift +. diffusion
    |> Float.min max_log_step
    |> Float.max (-.max_log_step)
  in
  let next_price = price *. Float.exp log_return in
  ensure_positive next_price

let simulate_path model ~initial_price ~noises =
  let len = Array.length noises in
  let path = Array.create ~len initial_price in
  let current = ref initial_price in
  for i = 0 to len - 1 do
    let next = step model ~price:!current ~noise:noises.(i) in
    path.(i) <- next;
    current := next
  done;
  path


