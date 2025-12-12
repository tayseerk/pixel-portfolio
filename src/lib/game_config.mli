type difficulty =
  | Easy
  | Medium
  | Hard
[@@deriving sexp, compare]

val default_state_file : string

(** Starting cash per difficulty. *)
val starting_cash : difficulty -> Money.cents

(** Default universe of assets. *)
val base_universe : Model.universe

val to_string : difficulty -> string
val of_string : string -> difficulty option
val all : difficulty list
