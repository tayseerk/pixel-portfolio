type t = string

let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_string
let t_of_sexp = Sexplib0.Sexp_conv.string_of_sexp

let compare = Stdlib.compare
let equal = String.equal

let hash_fold_t state t =
  Ppx_hash_lib.Std.Hash.Builtin.hash_fold_string state t

let hash t = Stdlib.Hashtbl.hash t

let of_string s = s
let to_string t = t

