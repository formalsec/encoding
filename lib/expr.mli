(** Term definitions of the abstract syntax *)
type inner = private
  { e : expr
  ; ty : Ty.t
  }

and t = inner Hc.hash_consed

and expr =
  | Val of Value.t
  | Ptr of int32 * t
  | Unop of Ty.unop * t
  | Binop of Ty.binop * t * t
  | Triop of Ty.triop * t * t * t
  | Relop of Ty.relop * t * t
  | Cvtop of Ty.cvtop * t
  | Symbol of Symbol.t
  | Extract of t * int * int
  | Concat of t * t

val ( @: ) : expr -> Ty.t -> t
val equal : t -> t -> bool
val hash : t -> int
val mk_symbol : Symbol.t -> t
val get_symbols : t list -> Symbol.t list
val pp : Format.formatter -> t -> unit
val pp_smt : Format.formatter -> t list -> unit
val pp_list : Format.formatter -> t list -> unit
val to_string : t -> string
val simplify : ?extract:bool -> t -> t

module H : sig
  val clear : unit -> unit
  val stats : unit -> Hashtbl.statistics
  val length : unit -> int
end

module Bool : sig
  val v : bool -> t
  val not : t -> t
  val ( = ) : t -> t -> t
  val ( != ) : t -> t -> t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
end

module Bitv : sig
  module I8 : Constructors_intf.Infix with type elt := int and type t := t
  module I32 : Constructors_intf.Infix with type elt := int32 and type t := t
  module I64 : Constructors_intf.Infix with type elt := int64 and type t := t
end

module Fpa : sig
  module F32 : Constructors_intf.Infix with type elt := float and type t := t
  module F64 : Constructors_intf.Infix with type elt := float and type t := t
end
