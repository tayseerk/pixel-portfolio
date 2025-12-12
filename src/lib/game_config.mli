(* Type: game difficulty levels *)
type difficulty =
  | Easy
  | Medium
  | Hard
[@@deriving sexp, compare]

(* Value: default save filename *)
val default_state_file : string

(* Function: initial cash per difficulty *)
val starting_cash : difficulty -> Money.cents

(* Value: base universe of assets *)
val base_universe : Model.universe

(* Function: convert difficulty to string *)
val to_string : difficulty -> string

(* Function: parse difficulty from string *)
val of_string : string -> difficulty option

(* Value: all difficulty values *)
val all : difficulty list
