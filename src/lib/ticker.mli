open Core

(* Type: stock/asset ticker symbol. *)
type t

(* Function: serialize ticker to sexp *)
val sexp_of_t : t -> Sexplib0.Sexp.t

(* Function: parse ticker from sexp *)
val t_of_sexp : Sexplib0.Sexp.t -> t

(* Function: compare ordering on tickers *)
val compare : t -> t -> int

(* Function: equality on tickers *)
val equal : t -> t -> bool

(* Function: hash a ticker *)
val hash : t -> int

(* Function: hash-fold for ppx_hash *)
val hash_fold_t :
  Ppx_hash_lib.Std.Hash.state ->
  t ->
  Ppx_hash_lib.Std.Hash.state

(* Function: build ticker from string *)
val of_string : string -> t

(* Function: get string from ticker *)
val to_string : t -> string

(** Map keyed by tickers. *)
module Map : Map.S with type Key.t = t


