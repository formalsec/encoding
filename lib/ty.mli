type _ cast =
  | C8 : int cast
  | C32 : int32 cast
  | C64 : int64 cast

type t =
  | Ty_int
  | Ty_real
  | Ty_bool
  | Ty_str
  | Ty_bitv of int
  | Ty_fp of int
  | Ty_list
  | Ty_tuple
  | Ty_array

type unop =
  | Neg
  | Not
  | Clz
  | Ctz
  (* Float *)
  | Abs
  | Sqrt
  | Is_nan
  | Ceil
  | Floor
  | Trunc
  | Nearest
  (* String *)
  | Seq_length (* (str.len String Int) *)
  | Seq_to_lowercase
  | Seq_to_uppercase
  | Trim (* uninterpreted *)

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
  (* String *)
  | Seq_at (* (str.at String Int String) *)
  | Seq_concat (* (str.substr String Int Int String) *)
  | Seq_prefix (* (str.prefixof String String Bool) *)
  | Seq_suffix (* (str.suffixof String String Bool) *)
  | Seq_contains (* (str.contains String String Bool) *)
  | Seq_last_index
  | Seq_split

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
  (* String *)
  | Seq_extract (* (str.substr String Int Int String) *)
  | Seq_replace (* (str.replace String String String String) *)
  | Seq_index (* (str.indexof String String Int Int) *)

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
  | Sign_extend of int
  | Zero_extend of int
  (* String *)
  | String_to_code (* (str.to_code String Int) *)
  | String_from_code (* (str.from_code Int String) *)
  | String_to_int (* (str.to_int String Int) *)
  | String_from_int (* (str.from_int Int String) *)
  | String_to_float

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

val equal : t -> t -> bool

val string_of_type : t -> string

val size : t -> int
