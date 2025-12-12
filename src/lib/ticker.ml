open Core

(* Type: ticker identifier (string). *)
type t = string
[@@deriving sexp, compare, equal, hash]

(* Function: convert from string to ticker. *)
let of_string s = s
(* Function: convert ticker to string. *)
let to_string t = t

module T = struct
  type nonrec t = t
  let compare = compare
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
end

(* Module: ticker-keyed Map. *)
module Map = Map.Make (T)


