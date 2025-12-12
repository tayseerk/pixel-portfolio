(** Abstract money type *)
type t

(** for backwards compatibility *)
type cents = t

(** S-expression helpers so other modules can derive sexp on [Money.cents] *)
val cents_of_sexp : Sexplib0.Sexp.t -> cents
val sexp_of_cents : cents -> Sexplib0.Sexp.t

val compare_cents : cents -> cents -> int

(** Construct from integer cents, e.g. 1023 -> $10.23. *)
val of_int_cents : int -> t

(** Extract raw cent amount. *)
val to_int_cents : t -> int

(** Convert a dollar float (e.g. 10.23) to cents, rounding to nearest cent. *)
val of_float_dollars : float -> t

(** Backwards-compatible helper name, used in tests. *)
val float_dollars_to_cents : float -> t

(** Convert a whole-dollar int (e.g. 10) to cents. *)
val of_int_dollars : int -> t

(** Convert cents back to a dollar float. *)
val to_float_dollars : t -> float

(** Add two Money values. *)
val addition : t -> t -> t

(** Subtract y from x. *)
val subtraction : t -> t -> t

(** Infix addition operator. *)
val ( $+ ) : t -> t -> t

(** Infix subtraction operator. *)
val ( $- ) : t -> t -> t

(** Multiply by an integer quantity (e.g. price * shares). *)
val multiply : t -> int -> t

(** Infix multiply operator. *)
val ( $* ) : t -> int -> t

(** Equality helper (Map.equal). *)
val equal : t -> t -> bool

val compare : t -> t -> int

(** Turn into a nice human-readable dollar string, e.g. "-10.23". *)
val make_it_look_nice : t -> string




