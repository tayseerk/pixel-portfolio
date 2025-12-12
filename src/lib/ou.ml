[@@@warning "-16"]
open Core

(* Type: OU parameters (kappa, theta, sigma, timestep dt). *)
type t = {
  kappa : float;
  theta : float;
  sigma : float;
  dt : float;
}
[@@deriving sexp]

(* Function: construct an OU model, default dt=1.0 *)
let create ~kappa ~theta ~sigma ?dt =
  let dt = Option.value dt ~default:1.0 in
  { kappa; theta; sigma; dt }


(* Value: floor to keep state positive *)
let min_state = 1e-6

(* Function: clamp state to stay positive *)
let ensure_positive state =
  (* keep the simulated state strictly positive to avoid negative prices *)
  Float.max state min_state

(* Function: one OU step with capped move size *)
let step t ~price ~noise =
  let mean_revert = t.kappa *. (t.theta -. price) *. t.dt in
  let diffusion = t.sigma *. Float.sqrt t.dt *. noise in
  let delta = mean_revert +. diffusion in
  (* allow sharper spikes: cap move to +/-40% of current price *)
  let max_step = 0.4 *. price in
  let capped_delta =
    delta
    |> Float.min max_step
    |> Float.max (-.max_step)
  in
  let next_price = price +. capped_delta in
  ensure_positive next_price

(* Function: simulate a path over a sequence of noises *)
let simulate_path model ~initial_state ~noises =
  let len = Array.length noises in
  let path = Array.create ~len initial_state in
  let current = ref initial_state in
  for i = 0 to len - 1 do
    let next = step model ~price:!current ~noise:noises.(i) in
    path.(i) <- next;
    current := next
  done;
  path


