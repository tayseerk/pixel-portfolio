open Core

type save_status =
  | Pass of string
  | Fail of string
[@@deriving sexp]

(* Persist engine state to a sexp file *)
val save_sexp : filename:string -> Engine.t -> save_status
(* Load engine state from a sexp file *)
val load_sexp : filename:string -> (Engine.t, string) Result.t
