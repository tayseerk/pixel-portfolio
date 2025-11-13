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


let step _ ~price ~noise:_ =
  (* No price dynamics yet; we simply keep the price constant. *)
  price

let simulate_path model ~initial_price ~noises =
  let len = Array.length noises in
  let path = Array.create ~len initial_price in
  for i = 0 to len - 1 do
    path.(i) <- step model ~price:initial_price ~noise:noises.(i)
  done;
  path


