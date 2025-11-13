(** params for a single Geometric Brownian Motion (GBM) process.

    We conceptually model a stock price with the stochastic
    differential equation:

    dS_t = μ S_t dt + σ S_t dW_t where:

    - (μ) is the drift, representing the expected
      rate of return of the asset per unit time.
    - (σ) is the volatility, controlling how noisy price is.
    - (dt) is the discrete time step used when we approximate the
      continuous-time process in code.

    t is used as a container for these and anything regarding the state is outside of the gbm value
*)

type t = {
  kappa : float;
  theta : float;
  sigma : float;
  dt : float;
}
[@@deriving sexp]

(** creates the gbm param *)
val create :
  kappa:float ->
  theta:float ->
  sigma:float ->
  ?dt:float ->
  t


  (* move one dt step from curr given price, essentially one tick*)
val step : t -> state:float -> noise:float -> float

(** creates the path the price should move for one asset*)
val simulate_path :
  t ->
  initial_state:float ->
  noises:float array ->
  float array

