open Ty

type inner =
  { e : expr
  ; ty : Ty.t
  }

and ht_expr = inner Hc.hash_consed

and expr =
  | Val of Value.t
  | Ptr of int32 * ht_expr
  | Unop of unop * ht_expr
  | Binop of binop * ht_expr * ht_expr
  | Triop of triop * ht_expr * ht_expr * ht_expr
  | Relop of relop * ht_expr * ht_expr
  | Cvtop of cvtop * ht_expr
  | Symbol of Symbol.t
  | Extract of ht_expr * int * int
  | Concat of ht_expr * ht_expr

module M = struct
  type t = inner

  let equal (e1 : inner) (e2 : inner) : bool =
    if not (e1.ty = e2.ty) then false
    else
      match (e1.e, e2.e) with
      | Val v1, Val v2 -> Value.equal v1 v2
      | Ptr (b1, o1), Ptr (b2, o2) -> b1 = b2 && o1 == o2
      | Unop (op1, e1), Unop (op2, e2) -> op1 = op2 && e1 == e2
      | Cvtop (op1, e1), Cvtop (op2, e2) -> op1 = op2 && e1 == e2
      | Binop (op1, e1, e3), Binop (op2, e2, e4) ->
        op1 = op2 && e1 == e2 && e3 == e4
      | Relop (op1, e1, e3), Relop (op2, e2, e4) ->
        op1 = op2 && e1 == e2 && e3 == e4
      | Triop (op1, e1, e3, e5), Triop (op2, e2, e4, e6) ->
        op1 = op2 && e1 == e2 && e3 == e4 && e5 == e6
      | Symbol s1, Symbol s2 -> Symbol.equal s1 s2
      | Extract (e1, h1, l1), Extract (e2, h2, l2) ->
        e1 == e2 && h1 = h2 && l1 = l2
      | Concat (e1, e3), Concat (e2, e4) -> e1 == e2 && e3 == e4
      | _ -> false

  let hash ({ e; ty } : inner) : int =
    let h x = Hashtbl.hash (ty, x) in
    match e with
    | Val v -> h v
    | Ptr (b, o) -> h (b, o.tag)
    | Unop (op, e) -> h (op, e.tag)
    | Cvtop (op, e) -> h (op, e.tag)
    | Binop (op, e1, e2) -> h (op, e1.tag, e2.tag)
    | Relop (op, e1, e2) -> h (op, e1.tag, e2.tag)
    | Triop (op, e1, e2, e3) -> h (op, e1.tag, e2.tag, e3.tag)
    | Symbol s -> h s
    | Extract (e, hi, lo) -> h (e.tag, hi, lo)
    | Concat (e1, e2) -> h (e1.tag, e2.tag)
end

let equal (e1 : ht_expr) (e2 : ht_expr) = e1.tag == e2.tag

module H = Hc.Make (M)

let ( @: ) e ty = H.hashcons { e; ty }
let mk_symbol s = Symbol s @: Symbol.type_of s

let is_num (e : ht_expr) =
  match e.node.e with Val (Num _) -> true | _ -> false

let get_symbols (hte : ht_expr list) =
  let tbl = Hashtbl.create 64 in
  let rec symbols (hte : ht_expr) =
    match hte.node.e with
    | Val _ -> ()
    | Ptr (_, offset) -> symbols offset
    | Unop (_, e1) -> symbols e1
    | Binop (_, e1, e2) ->
      symbols e1;
      symbols e2
    | Triop (_, e1, e2, e3) ->
      symbols e1;
      symbols e2;
      symbols e3
    | Relop (_, e1, e2) ->
      symbols e1;
      symbols e2
    | Cvtop (_, e) -> symbols e
    | Symbol s -> Hashtbl.replace tbl s ()
    | Extract (e, _, _) -> symbols e
    | Concat (e1, e2) ->
      symbols e1;
      symbols e2
  in
  List.iter symbols hte;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl []

module Pp = struct
  open Format

  let rec pp fmt (hte : ht_expr) =
    let ty = hte.node.ty in
    match hte.node.e with
    | Val v -> Value.pp fmt v
    | Ptr (base, offset) -> fprintf fmt "(Ptr (i32 %ld) %a)" base pp offset
    | Unop (op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp ty pp_unop op pp e
    | Binop (op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp ty pp_binop op pp e1 pp e2
    | Triop (op, e1, e2, e3) ->
      fprintf fmt "(%a.%a %a %a %a)" Ty.pp ty pp_triop op pp e1 pp e2 pp e3
    | Relop (op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp ty pp_relop op pp e1 pp e2
    | Cvtop (op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp ty pp_cvtop op pp e
    | Symbol s -> Symbol.pp fmt s
    | Extract (e, h, l) -> fprintf fmt "(extract %a %d %d)" pp e l h
    | Concat (e1, e2) -> fprintf fmt "(++ %a %a)" pp e1 pp e2

  let pp_list fmt (es : ht_expr list) =
    pp_print_list ~pp_sep:pp_print_space pp fmt es

  let pp_smt fmt (es : ht_expr list) : unit =
    let pp_symbols fmt syms =
      pp_print_list ~pp_sep:pp_print_newline
        (fun fmt sym ->
          let t = Symbol.type_of sym in
          fprintf fmt "(let-const %a %a)" Symbol.pp sym Ty.pp t )
        fmt syms
    in
    let pp_asserts fmt es =
      pp_print_list ~pp_sep:pp_print_newline
        (fun fmt e -> fprintf fmt "(assert @[<h 2>%a@])" pp e)
        fmt es
    in
    let syms = get_symbols es in
    if List.length syms > 0 then fprintf fmt "%a@\n" pp_symbols syms;
    if List.length es > 0 then fprintf fmt "%a@\n" pp_asserts es;
    pp_print_string fmt "(check-sat)"
end

let pp = Pp.pp
let pp_list = Pp.pp_list
let pp_smt = Pp.pp_smt
let to_string e = Format.asprintf "%a" pp e

let rec simplify_binop ty (op : binop) (e1 : ht_expr) (e2 : ht_expr) =
  match (e1.node.e, e2.node.e) with
  | Val (Num n1), Val (Num n2) -> Val (Num (Eval_numeric.eval_binop ty op n1 n2))
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    match op with
    | Sub when b1 = b2 -> simplify_binop ty Sub os1 os2
    | _ -> Binop (op, e1, e2) )
  | Ptr (base, offset), _ -> (
    match op with
    | Add ->
      let new_offset = simplify_binop offset.node.ty Add offset e2 in
      Ptr (base, new_offset @: offset.node.ty)
    | Sub ->
      let new_offset = simplify_binop offset.node.ty Sub offset e2 in
      Ptr (base, new_offset @: offset.node.ty)
    | Rem ->
      let rhs = Val (Value.Num (Num.I32 base)) @: ty in
      let addr = simplify_binop ty Add rhs offset @: ty in
      simplify_binop ty Rem addr e2
    | _ -> Binop (op, e1, e2) )
  | _, Ptr (base, offset) -> (
    match op with
    | Add ->
      let new_offset = simplify_binop offset.node.ty Add offset e1 in
      Ptr (base, new_offset @: offset.node.ty)
    | _ -> Binop (op, e1, e2) )
  | Val (Num (I32 0l)), _ -> (
    match op with
    | Add | Or -> e2.node.e
    | And | Div | DivU | Mul | Rem | RemU -> Val (Num (I32 0l))
    | _ -> Binop (op, e1, e2) )
  | _, Val (Num (I32 0l)) -> (
    match op with
    | Add | Or | Sub -> e1.node.e
    | And | Mul -> Val (Num (I32 0l))
    | _ -> Binop (op, e1, e2) )
  | Binop (op2, x, { node = { e = Val (Num v1); ty }; _ }), Val (Num v2)
    when not (is_num x) -> (
    match (op, op2) with
    | Add, Add ->
      let v = Eval_numeric.eval_binop ty Add v1 v2 in
      Binop (Add, x, Val (Num v) @: ty)
    (* | Add, Sub | Sub, Add -> *)
    (*   let v = Eval_numeric.eval_binop (I32 Sub) v1 v2 in *)
    (*   Binop (I32 Add, x, Val (Num v)) *)
    | Sub, Sub ->
      let v = Eval_numeric.eval_binop ty Add v1 v2 in
      Binop (Sub, x, Val (Num v) @: ty)
    | _, _ -> Binop (op, e1, e2) )
  | Binop (And, _, _), Val (Num (I32 1l)) -> e1.node.e
  | Val (Num (I32 1l)), Binop (And, _, _) -> e2.node.e
  | _ -> Binop (op, e1, e2)

let simplify_relop ty (op : relop) (e1 : ht_expr) (e2 : ht_expr) =
  match (e1.node.e, e2.node.e) with
  | Val (Num v1), Val (Num v2) ->
    Val (if Eval_numeric.eval_relop ty op v1 v2 then True else False)
  | Ptr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), Ptr (_, _) -> (
    match op with Eq -> Val False | Ne -> Val True | _ -> Relop (op, e1, e2) )
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    let v i = Val (Num (I32 i)) @: Ty_bitv S32 in
    match op with
    | Eq -> if b1 = b2 then Relop (Eq, os1, os2) else Val False
    | Ne -> if b1 = b2 then Relop (Ne, os1, os2) else Val True
    | (LtU | LeU | GtU | GeU) as op ->
      if b1 = b2 then Relop (op, os1, os2) else Relop (op, v b1, v b2)
    | _ -> Relop (op, e1, e2) )
  | _ -> Relop (op, e1, e2)

let nland64 (x : int64) (n : int) =
  let rec loop x' n' acc =
    if n' = 0 then Int64.logand x' acc
    else loop x' (n' - 1) Int64.(logor (shift_left acc 8) 0xffL)
  in
  loop x n 0L

let nland32 (x : int32) (n : int) =
  let rec loop x' n' acc =
    if n' = 0 then Int32.logand x' acc
    else loop x' (n' - 1) Int32.(logor (shift_left acc 8) 0xffl)
  in
  loop x n 0l

let simplify_extract (s : ht_expr) h l =
  match s.node.e with
  | Val (Num (I64 x)) ->
    let x' = nland64 (Int64.shift_right x (l * 8)) (h - l) in
    Val (Num (I64 x'))
  | _ -> if h - l = size s.node.ty then s.node.e else Extract (s, h, l)

let simplify_concat (msb : ht_expr) (lsb : ht_expr) =
  match (msb.node.e, lsb.node.e) with
  | ( Extract ({ node = { e = Val (Num (I64 x2)); _ }; _ }, h2, l2)
    , Extract ({ node = { e = Val (Num (I64 x1)); _ }; _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (Val (Num (I64 x)) @: msb.node.ty, d1 + d2, 0)
  | ( Extract ({ node = { e = Val (Num (I32 x2)); _ }; _ }, h2, l2)
    , Extract ({ node = { e = Val (Num (I32 x1)); _ }; _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland32 (Int32.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland32 (Int32.shift_right x2 (l2 * 8)) d2 in
    let x = Int32.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (Val (Num (I32 x)) @: msb.node.ty, d1 + d2, 0)
  | Extract (s1, h, m1), Extract (s2, m2, l) when equal s1 s2 && m1 = m2 ->
    Extract (s1, h, l)
  | ( Extract ({ node = { e = Val (Num (I64 x2)); ty }; _ }, h2, l2)
    , Concat
        ( { node =
              { e = Extract ({ node = { e = Val (Num (I64 x1)); _ }; _ }, h1, l1)
              ; _
              }
          ; _
          }
        , se ) )
    when not (is_num se) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    Concat (Extract (Val (Num (I64 x)) @: ty, d1 + d2, 0) @: ty, se)
  | _ -> Concat (msb, lsb)

let rec simplify ?(extract = true) (hte : ht_expr) : ht_expr =
  let ty = hte.node.ty in
  match hte.node.e with
  | Val _ -> hte
  | Ptr (base, offset) -> Ptr (base, simplify offset) @: ty
  | Binop (op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    simplify_binop ty op e1 e2 @: ty
  | Relop (op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    simplify_relop ty op e1 e2 @: ty
  | Extract (_, _, _) when not extract -> hte
  | Extract (s, h, l) when extract -> simplify_extract s h l @: ty
  | Concat (e1, e2) ->
    let msb = simplify ~extract:false e1 in
    let lsb = simplify ~extract:false e2 in
    simplify_concat msb lsb @: ty
  | _ -> hte

module Bool = struct
  let v b = (match b with true -> Val True | false -> Val False) @: Ty_bool

  let not (hte : ht_expr) =
    let ty = hte.node.ty in
    match hte.node.e with
    | Val True -> Val False @: Ty_bool
    | Val False -> Val True @: Ty_bool
    | Relop (Eq, e1, e2) -> Relop (Ne, e1, e2) @: ty
    | Relop (Ne, e1, e2) -> Relop (Eq, e1, e2) @: ty
    | Relop (Lt, e1, e2) -> Relop (Ge, e1, e2) @: ty
    | Relop (LtU, e1, e2) -> Relop (GeU, e1, e2) @: ty
    | Relop (Le, e1, e2) -> Relop (Gt, e1, e2) @: ty
    | Relop (LeU, e1, e2) -> Relop (GtU, e1, e2) @: ty
    | Relop (Gt, e1, e2) -> Relop (Le, e1, e2) @: ty
    | Relop (GtU, e1, e2) -> Relop (LeU, e1, e2) @: ty
    | Relop (Ge, e1, e2) -> Relop (Lt, e1, e2) @: ty
    | Relop (GeU, e1, e2) -> Relop (LtU, e1, e2) @: ty
    | _ -> Unop (Not, hte) @: Ty_bool

  let ( = ) (b1 : ht_expr) (b2 : ht_expr) =
    ( match (b1.node.e, b2.node.e) with
    | Val True, Val True | Val False, Val False -> Val True
    | _ -> Relop (Eq, b1, b2) )
    @: Ty_bool

  let ( != ) (b1 : ht_expr) (b2 : ht_expr) =
    ( match (b1.node.e, b2.node.e) with
    | Val True, Val False | Val False, Val True -> Val True
    | _ -> Relop (Ne, b1, b2) )
    @: Ty_bool

  let ( && ) (b1 : ht_expr) (b2 : ht_expr) =
    ( match (b1.node.e, b2.node.e) with
    | Val True, Val True -> Val True
    | Val False, Val True | Val True, Val False | Val False, Val False ->
      Val False
    | _ -> Binop (And, b1, b2) )
    @: Ty_bool

  let ( || ) (b1 : ht_expr) (b2 : ht_expr) =
    ( match (b1.node.e, b2.node.e) with
    | Val False, Val False -> Val False
    | Val True, Val True | Val False, Val True | Val True, Val False -> Val True
    | _ -> Binop (Or, b1, b2) )
    @: Ty_bool
end

module Make_bitv (T : sig
  type elt

  val ty : Ty.t
  val zero : elt
  val one : elt
  val value : elt -> ht_expr
end) =
struct
  open Hc

  let v i = T.value i [@@inline]
  let sym x = mk_symbol Symbol.(x @: T.ty) [@@inline]
  let ( ~- ) e = Unop (Neg, e) @: T.ty [@@inline]

  (* TODO: Do not normalize expressions such as x <= 0 or x = 0*)
  (* TODO: Do not create concrete abstract expressions *)
  (* TODO: pre-processing step to order variables *)
  (* I.e., (x > y) = (y < x) *)

  (* Normalize (x = y) = (x - y = 0) *)
  let ( = ) e1 e2 =
    match (e1.node.e, e2.node.e) with
    | _, Val (Num (I8 0 | I32 0l | I64 0L)) -> Relop (Eq, e1, e2) @: T.ty
    | _ -> Relop (Eq, Binop (Sub, e1, e2) @: T.ty, T.(value zero)) @: T.ty
  [@@inline]

  (* Canonize (x != y) = (x - y != 0) *)
  let ( != ) e1 e2 =
    match (e1.node.e, e2.node.e) with
    | _, Val (Num (I8 0 | I32 0l | I64 0L)) -> Relop (Ne, e1, e2) @: T.ty
    | _ -> Relop (Ne, Binop (Sub, e1, e2) @: T.ty, T.(value zero)) @: T.ty
  [@@inline]

  (* Canonize (x > y) = (-x < -y) = (y - x + 1 <= 0) *)
  let ( > ) e1 e2 =
    let lhs =
      match (e1.node.e, e2.node.e) with
      | _, Val (Num (I8 0 | I32 0l | I64 0L)) ->
        Binop (Sub, T.(value one), e1) @: T.ty
      | _ -> Binop (Add, Binop (Sub, e2, e1) @: T.ty, T.(value one)) @: T.ty
    in
    Relop (Le, lhs, T.(value zero)) @: T.ty
  [@@inline]

  (* Canonize (x >= y) = (-x <= -y) = (y - x <= 0) *)
  let ( >= ) e1 e2 =
    match (e1.node.e, e2.node.e) with
    | _, Val (Num (I8 0 | I32 0l | I64 0L)) ->
      Relop (Le, Unop (Neg, e1) @: T.ty, e2) @: T.ty
    | _ -> Relop (Le, Binop (Sub, e2, e1) @: T.ty, T.(value zero)) @: T.ty
  [@@inline]

  (* Canonize (x < y) = (x - y + 1 <= 0) *)
  let ( < ) e1 e2 =
    let lhs =
      match (e1.node.e, e2.node.e) with
      | _, Val (Num (I8 0 | I32 0l | I64 0L)) ->
        Binop (Add, e1, T.(value one)) @: T.ty
      | _ -> Binop (Add, Binop (Sub, e1, e2) @: T.ty, T.(value one)) @: T.ty
    in
    Relop (Le, lhs, T.(value zero)) @: T.ty
  [@@inline]

  (* Canonize (x <= y) = (x - y <= 0) *)
  let ( <= ) e1 e2 =
    match (e1.node.e, e2.node.e) with
    | _, Val (Num (I8 0 | I32 0l | I64 0L)) -> Relop (Le, e1, e2) @: T.ty
    | _ -> Relop (Le, Binop (Sub, e1, e2) @: T.ty, T.(value zero)) @: T.ty
  [@@inline]
end

module Bitv = struct
  module I8 = Make_bitv (struct
    type elt = int

    let ty = Ty_bitv S8
    let zero = 0
    let one = 1
    let value i = Val (Num (I8 i)) @: ty
  end)

  module I32 = Make_bitv (struct
    type elt = int32

    let ty = Ty_bitv S32
    let zero = 0l
    let one = 1l
    let value i = Val (Num (I32 i)) @: ty
  end)

  module I64 = Make_bitv (struct
    type elt = int64

    let ty = Ty_bitv S64
    let zero = 0L
    let one = 1L
    let value i = Val (Num (I64 i)) @: ty
  end)
end

module Make_fp (T : sig
  type elt

  val ty : Ty.t
  val value : elt -> ht_expr
end) =
struct
  let v i = T.value i [@@inline]
  let sym x = mk_symbol Symbol.(x @: T.ty) [@@inline]
  let ( ~- ) e = Unop (Neg, e) @: T.ty [@@inline]
  let ( = ) e1 e2 = Relop (Eq, e1, e2) @: T.ty [@@inline]
  let ( != ) e1 e2 = Relop (Ne, e1, e2) @: T.ty [@@inline]
  let ( > ) e1 e2 = Relop (Gt, e1, e2) @: T.ty [@@inline]
  let ( >= ) e1 e2 = Relop (Ge, e1, e2) @: T.ty [@@inline]
  let ( < ) e1 e2 = Relop (Lt, e1, e2) @: T.ty [@@inline]
  let ( <= ) e1 e2 = Relop (Le, e1, e2) @: T.ty [@@inline]
end

module Fpa = struct
  module F32 = Make_fp (struct
    type elt = float

    let ty = Ty_fp S32
    let value f = Val (Num (F32 (Int32.bits_of_float f))) @: ty
  end)

  module F64 = Make_fp (struct
    type elt = float

    let ty = Ty_fp S64
    let value f = Val (Num (F64 (Int64.bits_of_float f))) @: ty
  end)
end

type t = ht_expr

let hash (e : ht_expr) = e.tag
