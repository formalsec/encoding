type _ cast =
  | C8 : int cast
  | C32 : int32 cast
  | C64 : int64 cast

type bitwidth =
  | S8
  | S32
  | S64

type t =
  | Ty_int
  | Ty_real
  | Ty_bool
  | Ty_str
  | Ty_bitv of bitwidth
  | Ty_fp of bitwidth
  | Ty_var of { id : int }

type unop =
  | Abs
  | Ceil
  | Clz
  | Ctz
  | Floor
  | Is_nan
  | Len
  | Neg
  | Nearest
  | Not
  | Sqrt
  | Trim
  | Trunc

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | DivU
  | Rem
  | RemU
  | Shl
  | ShrA
  | ShrL
  | And
  | Or
  | Xor
  | Pow
  | Min
  | Max
  | Rotl
  | Rotr
  (* To remove *)
  | Nth
  | Concat

type relop =
  | Eq
  | Ne
  | Lt
  | LtU
  | Gt
  | GtU
  | Le
  | LeU
  | Ge
  | GeU

type triop =
  | Ite
  | Substr

type cvtop =
  | ToString
  | OfString
  | ToBool
  | OfBool
  | Reinterpret_int
  | Reinterpret_float
  | DemoteF64
  | PromoteF32
  | ConvertSI32
  | ConvertUI32
  | ConvertSI64
  | ConvertUI64
  | TruncSF32
  | TruncUF32
  | TruncSF64
  | TruncUF64
  | WrapI64
  | ExtS of int
  | ExtU of int
  | String_to_code
  | String_from_code

type logic =
  | AUFLIA
  | AUFLIRA
  | AUFNIRA
  | LIA
  | LRA
  | QF_ABV
  | QF_AUFBV
  | QF_AUFLIA
  | QF_AX
  | QF_BV
  | QF_BVFP
  | QF_IDL
  | QF_LIA
  | QF_LRA
  | QF_NIA
  | QF_NRA
  | QF_RDL
  | QF_UF
  | QF_UFBV
  | QF_UFIDL
  | QF_UFLIA
  | QF_UFLRA
  | QF_UFNRA
  | UFLRA
  | UFNIA

val pp_unop : Format.formatter -> unop -> unit
val pp_binop : Format.formatter -> binop -> unit
val pp_triop : Format.formatter -> triop -> unit
val pp_relop : Format.formatter -> relop -> unit
val pp_cvtop : Format.formatter -> cvtop -> unit
val pp : Format.formatter -> t -> unit
val pp_logic : Format.formatter -> logic -> unit
val string_of_type : t -> string
val size : t -> int
