open Core

(** Upper bound on simulated steps per command. *)
val max_sim_steps : int

(** Build a fresh engine configured for the selected difficulty. *)
val create_engine : Game_config.difficulty -> Engine.t

(** Save engine state to a sexp file. *)
val save_engine : state:string -> Engine.t -> unit Or_error.t

(** Load engine state from file, reconcile open orders, and resave. *)
val load_engine : state:string -> Engine.t Or_error.t

(** Build and submit an order, saving state and printing status. *)
val place_order :
  state:string ->
  ticker:string ->
  qty:int ->
  price_opt:float option ->
  side:Order.side ->
  unit Or_error.t

(** Advance N steps, save, and print price deltas + portfolio. *)
val simulate : state:string -> steps:int -> unit Or_error.t

(** Start a new game, backing up any existing state, and print status. *)
val new_game :
  state:string ->
  mode:Game_config.difficulty ->
  Engine.t Or_error.t

(** Save current game state to a new target file. *)
val save_as : state:string -> target:string -> unit Or_error.t

(** Load a state from [source] into [state], backing up current. *)
val load_into :
  state:string ->
  source:string ->
  Engine.t Or_error.t

(** Cancel an open order by id and save the updated state. *)
val cancel_order_cmd :
  state:string ->
  order_id:int ->
  unit Or_error.t
