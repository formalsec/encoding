module type S = sig
  type t
  type solver

  (** Time spent inside SMT solver. *)
  val solver_time : float ref

  (** Number of queries to the SMT solver. *)
  val solver_count : int ref

  (** Print solver statistics. *)
  val pp_statistics : Format.formatter -> t -> unit

  (** [create ?params ?logic ()] creates a new solver.

      [?params] is of type {!type:Params.t} and is used to modify/set parameters
      inside the solver.

      [?logic] is of type {!type:Solver_intf.logic} and is used to set the
      theory of the assertions used. When knowing what the underlying theory is
      going to be, setting this parameter can help the SMT solver be more
      performant. The default logic is {e unknown_theory}. *)
  val create : ?params:Params.t -> ?logic:Ty.logic -> unit -> t

  (** Interrupt solver. *)
  val interrupt : unit -> unit

  (** Clone a given solver. *)
  val clone : t -> t

  (** Create a backtracking point. *)
  val push : t -> unit

  (** [pop solver n] backtracks [n] backtracking points. *)
  val pop : t -> int -> unit

  (** Resets the solver, i.e., remove all assertions from the solver. *)
  val reset : t -> unit

  (** Assert one or multiple constraints into the solver. *)
  val add : t -> Expr.t list -> unit

  (** The set of assertions in the solver. *)
  val get_assertions : t -> Expr.t list

  (** [check solver es] checks the satisfiability of the assertions in the
      solver using the assumptions in [es].

      Raises [Unknown] if the SMT solver returns unknown. *)
  val check : t -> Expr.t list -> bool

  (** [get_value solver e] get an expression denoting the model value of a given
      expression.

      Requires that the last {!val:check} query returned [true]. *)
  val get_value : t -> Expr.t -> Expr.t

  (** The model of the last [check].

      The result is [None] if [check] was not invoked before, or its result was
      not [Satisfiable]. *)
  val model : ?symbols:Symbol.t list -> t -> Model.t option
end

module type S_cached = sig
  include S
  module Cache : Cache_intf.S
end

module type Intf = sig
  module type S = S

  (** The Encoding module defines two types of solvers: {!module:Batch} and
      {!module:Incremental}. The generic definition of these solvers is
      presented here, and they are parametric on the mappings of the underlying
      SMT solver. This design allows for the creation of portable solvers that
      can be used with various SMT solvers implementing
      {!module-type:Mappings_intf.S}.

      {1 Batch Mode}

      In this module, constraints are handled in a 'batch' mode, meaning that
      the solver delays all interactions with the underlying SMT solver until it
      becomes necessary. It essentially communicates with the underlying solver
      only when a call to {!val:Solver_intf.S.check},
      {!val:Solver_intf.S.get_value}, or {!val:Solver_intf.S.model} is made. *)

  (** {!module:Batch} is parameterized by the mapping module [M] implementing
      {!module-type:Mappings_intf.S}. In this mode, the solver delays all
      interactions with the underlying SMT solver until it becomes necessary. *)
  module Batch (M : Mappings_intf.S) : S

  module Batch_cached (M : Mappings_intf.S) : S_cached

  (** {1 Incremental Model}

      In the Incremental module, constraints are managed incrementally,
      signifying that upon their addition to the solver, this module promptly
      communicates with the underlying SMT solver. Unlike the batch solver,
      nearly every interaction with this solver prompts a corresponding
      interaction with the underlying SMT solver. *)

  (** The {!module:Incremental} module, akin to {!module:Batch}, presents a
      solver parameterized by the mapping module [M]. In this mode, the
      Incremental solver engages with the underlying SMT solver in nearly every
      interaction. *)
  module Incremental (M : Mappings_intf.S) : S

  (** {!module:Z3_batch} is a concrete instantiation of {!module:Batch} with
      {!module:Z3_mappings}, providing a solver specifically tailored for the Z3
      SMT solver. *)
  module Z3_batch : S

  (** {!module:Z3_incremental} is a specific instantiation of
      {!module:Incremental} with {!module:Z3_mappings}, creating an incremental
      solver designed for the Z3 SMT solver.*)
  module Z3_incremental : S
end
