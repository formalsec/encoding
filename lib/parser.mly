%{
open Value
open Ty
open Expr

let varmap = Hashtbl.create 512

let add_bind x t = Hashtbl.replace varmap x t
let get_bind x = Hashtbl.find varmap x

%}
%token PTR
%token EXTRACT
%token CONCAT
%token LPAREN
%token RPAREN
%token ASSERT
%token DECLARE_FUN CHECK_SAT GET_MODEL
(*%token HOLE*)
%token EOF

%token <int> NUM
%token <float> DEC
%token <bool> BOOL
%token <string> STR
%token <string> SYMBOL
%token <Ty.t * Ty.unop> UNARY
%token <Ty.t * Ty.binop> BINARY
%token <Ty.t * Ty.triop> TERNARY
%token <Ty.t * Ty.relop> RELOP
%token <Ty.t * Ty.cvtop> CVTOP
%token <Ty.t> TYPE

%start <Ast.t list> script
%%

let script := stmts = list(stmt); EOF; { stmts }

let stmt :=
  | LPAREN; DECLARE_FUN; x = SYMBOL; t = TYPE; RPAREN;
    {
      add_bind x t;
      Ast.Declare (Symbol.mk_symbol t x)
    }
  | LPAREN; ASSERT; e = s_expr; RPAREN; { Ast.Assert e }
  | LPAREN; CHECK_SAT; RPAREN; { Ast.CheckSat }
  | LPAREN; GET_MODEL; RPAREN; { Ast.GetModel }

let s_expr :=
  | x = SYMBOL; { mk_symbol @@ Symbol.mk_symbol (get_bind x) x }
  | c = spec_constant; { Val c @: Value.type_of c }
  | LPAREN; op = paren_op; RPAREN; { op }

let paren_op :=
  | PTR; LPAREN; ty = TYPE; x = NUM; RPAREN; e = s_expr;
    { Ptr (Int32.of_int x, e) @: ty }
  | (ty, op) = UNARY; e = s_expr;
    { Unop (op, e) @: ty }
  | (ty, op) = BINARY; e1 = s_expr; e2 = s_expr;
    { Binop (op, e1, e2) @: ty }
  | (ty, op) = TERNARY; e1 = s_expr; e2 = s_expr; e3 = s_expr;
    { Triop (op, e1, e2, e3) @: ty }
  | (ty, op) = CVTOP; e = s_expr;
    { Cvtop (op, e) @: ty }
  | (ty, op) = RELOP; e1 = s_expr; e2 = s_expr;
    { Relop (op, e1, e2) @: ty }
  | EXTRACT; e = s_expr; l = NUM; h = NUM;
    { Extract (e, h, l) @: Ty_bitv S32 }
  | CONCAT; e1 = s_expr; e2 = s_expr;
    { Concat (e1, e2) @: Ty_bitv S32 }

let spec_constant :=
  | x = NUM; { Int x }
  | x = DEC; { Real x }
  | x = STR; { Str x }
  | x = BOOL; { if x then True else False }
  | LPAREN; ty = TYPE; x = NUM; RPAREN;
    {
      match ty with
      | Ty_bitv S32 -> Num (I32 (Int32.of_int x))
      | Ty_bitv S64 -> Num (I64 (Int64.of_int x))
      | _ -> failwith "invalid bitv type"
    }
  | LPAREN; ty = TYPE; x = DEC; RPAREN;
    {
      match ty with
      | Ty_fp S32 -> Num (F32 (Int32.bits_of_float x))
      | Ty_fp S64 -> Num (F64 (Int64.bits_of_float x))
      | _ -> failwith "invalid fp type"
    }
