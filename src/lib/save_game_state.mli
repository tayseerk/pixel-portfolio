open Core

(* Type: success/failure with message. *)
type save_status =
  | Pass of string
  | Fail of string
[@@deriving sexp]

(* Function: persist/save engine state to a sexp file *)
val save_sexp : filename:string -> Engine.t -> save_status
(* Function: load engine state from a sexp file *)
val load_sexp : filename:string -> (Engine.t, string) Result.t
