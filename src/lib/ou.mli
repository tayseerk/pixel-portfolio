(** Parameters for a single Ornstein–Uhlenbeck (OU) mean-reverting process.

    OU models mean-reverting dynamics:
      dX_t = κ (θ - X_t) dt + σ dW_t

    - κ (kappa): speed of mean reversion toward θ.
    - θ (theta): long-run mean level.
    - σ (sigma): volatility / noise scale.
    - dt: discrete timestep used to approximate the continuous process.
*)

(* Type: OU parameters (kappa, theta, sigma, timestep dt). *)
type t = {
  kappa : float;
  theta : float;
  sigma : float;
  dt : float;
}
[@@deriving sexp]

(* Function: construct an OU spec (dt defaults to 1.0) *)
val create :
  kappa:float ->
  theta:float ->
  sigma:float ->
  ?dt:float ->
  t

(* Function: step one OU step given current price and noise *)
val step : t -> price:float -> noise:float -> float

(* Function: simulate OU path over provided noises *)
val simulate_path :
  t ->
  initial_state:float ->
  noises:float array ->
  float array

