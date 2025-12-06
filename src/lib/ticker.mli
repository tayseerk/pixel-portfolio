(* Stock/asset ticker symbol *)
type t = string

(* Sexp/compare/hash helpers *)
val sexp_of_t : t -> Sexplib0.Sexp.t
val t_of_sexp : Sexplib0.Sexp.t -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val hash_fold_t :
  Ppx_hash_lib.Std.Hash.state ->
  t ->
  Ppx_hash_lib.Std.Hash.state

val of_string : string -> t
val to_string : t -> string

