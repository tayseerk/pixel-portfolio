open Core

(** balance converted to cents $10.23 -> 1023 *)
type cents = int [@@deriving sexp, compare, hash]

(** to convert 10.23 to cents 1023, rounds to the nearest cent *)
val float_dollars_to_cents : float -> cents

(** convert cents back to a dollar float *)
val cents_to_float_dollars : cents -> float

(** parse through a string i.e 10.23 (dollar float) and convert to cents *)
val string_dollars : string -> (cents, string) Result.t

(** add*)
val addition : cents -> cents -> cents

(** subtract *)
val subtraction : cents -> cents -> cents

(** multiply *)
val multiply : cents -> int -> cents

(** turn back into a nice readable string *)
val make_it_look_nice : cents -> string



