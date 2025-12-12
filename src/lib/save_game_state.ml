open Core

(* Type: success/failure with message *)
type save_status =
  | Pass of string
  | Fail of string
[@@deriving sexp]

(* Function: persist/save engine state to a sexp file *)
let save_sexp ~filename engine =
  match Or_error.try_with (fun () ->
            Sexp.save_hum filename (Engine.sexp_of_t engine)) with
  | Ok () ->
      Pass (sprintf "Saved current game to %s" filename)
  | Error err -> Fail (Error.to_string_hum err)

(* Function: load engine state from a sexp file *)
let load_sexp ~filename =
  match Or_error.try_with (fun () -> Sexp.load_sexp filename) with
  | Error err -> Error (Error.to_string_hum err)
  | Ok sexp -> (
      try Ok (Engine.t_of_sexp sexp)
      with exn -> Error (Exn.to_string exn))
