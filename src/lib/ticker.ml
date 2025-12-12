open Core

type t = string
[@@deriving sexp, compare, equal, hash]

let of_string s = s
let to_string t = t

module T = struct
  type nonrec t = t
  let compare = compare
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
end

module Map = Map.Make (T)


