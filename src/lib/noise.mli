open Core

(** Maximum absolute noise applied to the underlying stochastic process. *)
val max_abs_noise : float

(** Sample a single noise value in [-max_abs_noise, +max_abs_noise]. *)
val sample : unit -> float

(** Ensure there is a noise value for every ticker in the universe. *)
val ensure_noise_map :
  universe:Model.universe ->
  existing:float String.Map.t ->
  float String.Map.t
