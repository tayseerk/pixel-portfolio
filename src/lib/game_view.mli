(** Pretty-print a Money.cents value as a dollar string. *)
val pretty_money : Money.cents -> string

(** Print current prices with deltas vs initial prices. *)
val print_prices : Engine.t -> unit

(** Print cash, equity, positions, and open orders. *)
val print_positions : Engine.t -> unit
