open Format

type _ cast =
  | C32 : int32 cast
  | C64 : int64 cast

type sz =
  | S8
  | S32
  | S64

type t =
  | Ty_int
  | Ty_real
  | Ty_bool
  | Ty_str
  | Ty_bitv of sz
  | Ty_fp of sz

type unop =
  | Neg
  | Not
  | Clz
  | Abs
  | Sqrt
  | Nearest
  | Is_nan
  | Ceil
  | Floor
  (* To remove *)
  | Len
  | Trim

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

let pp_string = pp_print_string

let pp_unop fmt (op : unop) =
  match op with
  | Neg -> pp_string fmt "neg"
  | Not -> pp_string fmt "not"
  | Clz -> pp_string fmt "clz"
  | Abs -> pp_string fmt "abs"
  | Sqrt -> pp_string fmt "sqrt"
  | Nearest -> pp_string fmt "nearest"
  | Is_nan -> pp_string fmt "is_nan"
  | Ceil -> pp_string fmt "ceil"
  | Floor -> pp_string fmt "floor"
  | Len -> pp_string fmt "len"
  | Trim -> pp_string fmt "trim"

let pp_binop fmt (op : binop) =
  match op with
  | Add -> pp_string fmt "add"
  | Sub -> pp_string fmt "sub"
  | Mul -> pp_string fmt "mul"
  | Div -> pp_string fmt "div"
  | DivU -> pp_string fmt "div_u"
  | Rem -> pp_string fmt "rem"
  | RemU -> pp_string fmt "rem_u"
  | Shl -> pp_string fmt "shl"
  | ShrA -> pp_string fmt "shr"
  | ShrL -> pp_string fmt "shr_u"
  | And -> pp_string fmt "and"
  | Or -> pp_string fmt "or"
  | Xor -> pp_string fmt "xor"
  | Pow -> pp_string fmt "pow"
  | Min -> pp_string fmt "min"
  | Max -> pp_string fmt "max"
  | Rotl -> pp_string fmt "rotl"
  | Rotr -> pp_string fmt "rotr"
  | Nth -> pp_string fmt "nth"
  | Concat -> pp_string fmt "++"

let pp_triop fmt (op : triop) =
  match op with Ite -> pp_string fmt "ite" | Substr -> pp_string fmt "sub"

let pp_relop fmt (op : relop) =
  match op with
  | Eq -> pp_string fmt "eq"
  | Ne -> pp_string fmt "ne"
  | Lt -> pp_string fmt "lt"
  | LtU -> pp_string fmt "lt_u"
  | Gt -> pp_string fmt "gt"
  | GtU -> pp_string fmt "gt_u"
  | Le -> pp_string fmt "le"
  | LeU -> pp_string fmt "le_u"
  | Ge -> pp_string fmt "ge"
  | GeU -> pp_string fmt "ge_u"

let pp_cvtop fmt (op : cvtop) =
  match op with
  | ToString -> pp_string fmt "to_string"
  | OfString -> pp_string fmt "of_string"
  | ToBool -> pp_string fmt "to_bool"
  | OfBool -> pp_string fmt "of_bool"
  | Reinterpret_int -> pp_string fmt "reinterpret_int"
  | Reinterpret_float -> pp_string fmt "reinterpret_float"
  | DemoteF64 -> pp_string fmt "demote_f64"
  | PromoteF32 -> pp_string fmt "promote_f32"
  | ConvertSI32 -> pp_string fmt "convert_i32_s"
  | ConvertUI32 -> pp_string fmt "convert_i32_u"
  | ConvertSI64 -> pp_string fmt "convert_i64_s"
  | ConvertUI64 -> pp_string fmt "convert_i64_u"
  | TruncSF32 -> pp_string fmt "trunc_f32_s"
  | TruncUF32 -> pp_string fmt "trunc_f32_u"
  | TruncSF64 -> pp_string fmt "trunc_f64_s"
  | TruncUF64 -> pp_string fmt "trunc_f64_u"
  | WrapI64 -> pp_string fmt "wrap_i64"
  | ExtS sz -> fprintf fmt "extend_i%d_s" sz
  | ExtU sz -> fprintf fmt "extend_i%d_u" sz
  | String_to_code -> pp_string fmt "string_to_code"
  | String_from_code -> pp_string fmt "string_from_code"

let pp fmt = function
  | Ty_int -> pp_string fmt "int"
  | Ty_real -> pp_string fmt "real"
  | Ty_bool -> pp_string fmt "bool"
  | Ty_str -> pp_string fmt "str"
  | Ty_bitv S8 -> pp_string fmt "i8"
  | Ty_bitv S32 -> pp_string fmt "i32"
  | Ty_bitv S64 -> pp_string fmt "i64"
  | Ty_fp S8 -> pp_string fmt "f8"
  | Ty_fp S32 -> pp_string fmt "f32"
  | Ty_fp S64 -> pp_string fmt "f64"

let string_of_type (ty : t) : string = Format.asprintf "%a" pp ty

let size (ty : t) : int =
  match ty with
  | Ty_bitv S32 | Ty_fp S32 -> 4
  | Ty_bitv S64 | Ty_fp S64 -> 8
  | _ -> assert false
