open Smtlib
open Syntax.Result

let ty_env : (string, Ty.t) Hashtbl.t = Hashtbl.create 16

let rewrite : (term, sort) command -> ((Expr.t, Ty.t) command, string) Result.t
    = function
  | Assert term ->
    let* term = Expr.Smtlib.expr_of_term ty_env term in
    Ok (Assert term)
  | Check_sat -> Ok Check_sat
  | Check_sat_assuming -> Ok Check_sat_assuming
  | Declare_const (sym, sort) ->
    let* ty = Expr.Smtlib.type_of_sort sort in
    Hashtbl.add ty_env sym ty;
    Ok (Declare_const (sym, ty))
  | Declare_datatype -> Ok Declare_datatype
  | Declare_datatypes -> Ok Declare_datatypes
  | Declare_fun (sym, args, ret) ->
    let* args = list_map ~f:Expr.Smtlib.type_of_sort args in
    let* ret = Expr.Smtlib.type_of_sort ret in
    Ok (Declare_fun (sym, args, ret))
  | Declare_sort (sym, n) -> Ok (Declare_sort (sym, n))
  | Define_fun -> Ok Define_fun
  | Define_fun_rec -> Ok Define_fun_rec
  | Define_funs_rec -> Ok Define_funs_rec
  | Define_sort -> Ok Define_sort
  | Echo msg -> Ok (Echo msg)
  | Exit -> Ok Exit
  | Get_assertions -> Ok Get_assertions
  | Get_assignment -> Ok Get_assignment
  | Get_info -> Ok Get_info
  | Get_model -> Ok Get_model
  | Get_option opt -> Ok (Get_option opt)
  | Get_proof -> Ok Get_proof
  | Get_unsat_assumptions -> Ok Get_unsat_assumptions
  | Get_unsat_core -> Ok Get_unsat_core
  | Get_value terms ->
    let* terms = list_map ~f:(Expr.Smtlib.expr_of_term ty_env) terms in
    Ok (Get_value terms)
  | Pop n -> Ok (Pop n)
  | Push n -> Ok (Push n)
  | Reset -> Ok Reset
  | Reset_assertions -> Ok Reset_assertions
  | Set_info attr -> Ok (Set_info attr)
  | Set_logic log -> Ok (Set_logic log)
  | Set_option -> Ok Set_option

let script script =
  Hashtbl.reset ty_env;
  Format.printf "Rewriting ...@.";
  list_map ~f:rewrite script
