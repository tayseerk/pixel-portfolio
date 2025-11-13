open Core

type save_status =
  | Pass of string
  | Fail of string
[@@deriving sexp]

val save_sexp : filename:string -> Engine.t -> save_status
val load_sexp : filename:string -> (Engine.t, string) Result.t

val save_json : filename:string -> Engine.t -> save_status
val load_json : filename:string -> (Engine.t, string) Result.t
