(* GBM model parameters *)
type t = {
  mu : float;    (* drift *)
  sigma : float; (* volatility *)
  dt : float;    (* time step *)
}
[@@deriving sexp]

(* Build a GBM spec *)
val create : mu:float -> sigma:float -> ?dt:float -> t

(* One-step GBM update *)
val step : t -> price:float -> noise:float -> float

(* Simulate a GBM path over provided noises *)
val simulate_path :
  t ->
  initial_price:float ->
  noises:float array ->
  float array

