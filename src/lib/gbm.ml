[@@@warning "-16"]
open Core

(* Type: GBM parameters (drift mu, volatility sigma, timestep dt). *)
type t = {
  mu : float;
  sigma : float;
  dt : float;
}
[@@deriving sexp]

(* Function: construct a GBM model; dt (timestep size for gbm model) defaults to 1.0. *)
let create ~mu ~sigma ?dt =
  let dt = Option.value dt ~default:1.0 in
  { mu; sigma; dt }


(* Value: floor to keep simulated prices positive. *)
let min_price = 1e-6

(* Function: clamp price to stay positive. *)
let ensure_positive price =
  (* keep the simulated price strictly positive to avoid downstream issues *)
  Float.max price min_price

(* Function: one GBM step with capped log-return to limit jumps. *)
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

(* Function: simulate a price path over a sequence of noises *)
let simulate_path model ~initial_price ~noises =
  let _, reversed_path =
    List.fold_left noises ~init:(initial_price, []) ~f:(fun (current, acc) noise ->
        let next = step model ~price:current ~noise in
        (next, next :: acc))
  in
  List.rev reversed_path


