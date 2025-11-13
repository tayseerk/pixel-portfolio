type t = {
  mu : float;
  sigma : float;
  dt : float;
}
[@@deriving sexp]

val create : mu:float -> sigma:float -> ?dt:float -> t

val step : t -> price:float -> noise:float -> float

val simulate_path :
  t ->
  initial_price:float ->
  noises:float array ->
  float array

