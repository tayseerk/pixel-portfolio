open Core

type cents = int [@@deriving sexp, compare, hash]

let float_dollars_to_cents f =
  Float.(round_nearest (f *. 100.0)) |> Int.of_float

let cents_to_float_dollars c =
  Float.of_int c /. 100.0

let string_dollars s =
  match Float.of_string_opt s with
  | None -> Error ("could not parse dollars: " ^ s)
  | Some f -> Ok (float_dollars_to_cents f)

let addition (x : cents) (y : cents) : cents = x + y
let subtraction (x : cents) (y : cents) : cents = x - y
let multiply (amount : cents) (d : int) : cents = amount * d

let make_it_look_nice c =
  let sign, abs_val_c =
    if c < 0 then "-", Int.neg c else "", c
  in
  let dollars = abs_val_c / 100 in
  let cents = abs_val_c mod 100 in
  sprintf "%s%d.%02d" sign dollars cents

