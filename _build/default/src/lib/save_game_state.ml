open Core

type save_status =
  | Pass of string
  | Fail of string
[@@deriving sexp]

let save_sexp ~filename _engine =
  (* Placeholder; real implementation will serialise Engine.t as an s-expression. *)
  let msg = sprintf "Saving to %s is not implemented yet." filename in
  Fail msg

let load_sexp ~filename =
  let msg = sprintf "Loading from %s is not implemented yet." filename in
  Error msg

let save_json ~filename _engine =
  let msg = sprintf "Saving JSON to %s is not implemented yet." filename in
  Fail msg

let load_json ~filename =
  let msg = sprintf "Loading JSON from %s is not implemented yet." filename in
  Error msg
