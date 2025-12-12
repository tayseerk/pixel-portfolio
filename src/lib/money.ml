open Core

(* Type: concrete representation for money (cents as int) *)
type t = int [@@deriving sexp, compare, hash]

(* Alias: cents — backward-compatible name *)
type cents = t

(* helpers so [@@deriving sexp] works for cents *)
let cents_of_sexp = t_of_sexp  (* parse cents from sexp *)
let sexp_of_cents = sexp_of_t  (* emit cents to sexp *)

(* Comparator alias for cents *)
let compare_cents = compare

(* Conversions to/from ints *)
let of_int_cents (c : int) : t = c
let to_int_cents (c : t) : int = c

(* Convert float dollars to cents (rounded nearest) *)
let of_float_dollars (f : float) : t =
  Float.(round_nearest (f *. 100.0)) |> Int.of_float

(* Convert int dollars to cents *)
let of_int_dollars (d : int) : t =
  of_float_dollars (Float.of_int d)

(* Convert cents to float dollars *)
let to_float_dollars (c : t) : float =
  Float.of_int c /. 100.0

(* Basic arithmetic in cents *)
let addition (x : t) (y : t) : t = x + y
let subtraction (x : t) (y : t) : t = x - y

let ( $+ ) = addition
let ( $- ) = subtraction

(* Multiply cents by an int quantity *)
let multiply (amount : t) (d : int) : t = amount * d
let ( $* ) = multiply

(* Backward-compatible name used in tests / older code *)
let float_dollars_to_cents = of_float_dollars

(* Equality helper for tests/maps *)
let equal (x : t) (y : t) = Int.equal x y

(* Pretty-print cents as dollars string. *)
let make_it_look_nice (c : t) =
  let sign, abs_val_c =
    if c < 0 then "-", Int.neg c else "", c
  in
  let dollars = abs_val_c / 100 in
  let cents = abs_val_c mod 100 in
  sprintf "%s%d.%02d" sign dollars cents




