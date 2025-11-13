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



let step _ ~state ~noise:_ =
  (* No mean-reversion dynamics yet; we simply keep the state constant. *)
  state

let simulate_path model ~initial_state ~noises =
  let len = Array.length noises in
  let path = Array.create ~len initial_state in
  for i = 0 to len - 1 do
    path.(i) <- step model ~state:initial_state ~noise:noises.(i)
  done;
  path


