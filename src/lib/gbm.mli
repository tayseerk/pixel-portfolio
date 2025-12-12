(** Parameters for a single Geometric Brownian Motion (GBM) process.

    We model stock price S with:
    {v dS_t = μ S_t dt + σ S_t dW_t v}

    - μ (mu): drift / expected return per unit time.
    - σ (sigma): volatility / noise scale.
    - dt: discrete timestep used to approximate the continuous process.

    [t] just holds these parameters; price state lives outside [t]. *)
    
(* Type: GBM model parameters. *)
type t = {
  mu : float;    (* drift / average growth rate *)
  sigma : float; (* volatility / randomness scale *)
  dt : float;    (* time step size *)
}
[@@deriving sexp]

(* Function: build a GBM spec (dt defaults to 1.0) *)
val create : mu:float -> sigma:float -> ?dt:float -> t

(* Function: one-step GBM update given price and noise *)
val step : t -> price:float -> noise:float -> float

(* Function: simulate a GBM path over provided noises *)
val simulate_path :
  t ->
  initial_price:float ->
  noises:float array ->
  float array

