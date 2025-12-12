open Core

(* Concrete rep*)
type t = int [@@deriving sexp, compare, hash]

(* Backwards-compatible alias for older code that used [Money.cents]. *)
type cents = t

(* S-expression helpers for [cents], needed by [@@deriving sexp] in other modules. *)
let cents_of_sexp = t_of_sexp
let sexp_of_cents = sexp_of_t

let compare_cents = compare

let of_int_cents (c : int) : t = c
let to_int_cents (c : t) : int = c

let of_float_dollars (f : float) : t =
  Float.(round_nearest (f *. 100.0)) |> Int.of_float

let of_int_dollars (d : int) : t =
  of_float_dollars (Float.of_int d)

let to_float_dollars (c : t) : float =
  Float.of_int c /. 100.0

let addition (x : t) (y : t) : t = x + y
let subtraction (x : t) (y : t) : t = x - y

let ( $+ ) = addition
let ( $- ) = subtraction

let multiply (amount : t) (d : int) : t = amount * d
let ( $* ) = multiply

(* Backwards-compatible name used in tests / older code. *)
let float_dollars_to_cents = of_float_dollars

(* Simple equality for tests / maps. *)
let equal (x : t) (y : t) = Int.equal x y

let make_it_look_nice (c : t) =
  let sign, abs_val_c =
    if c < 0 then "-", Int.neg c else "", c
  in
  let dollars = abs_val_c / 100 in
  let cents = abs_val_c mod 100 in
  sprintf "%s%d.%02d" sign dollars cents




