type status =
  | Sat
  | Unsat
  | Unknown

type script = (term, sort) command list

and ('term, 'sort) command =
  | Assert of 'term
  | Check_sat
  | Check_sat_assuming
  | Declare_const of symbol * 'sort
  | Declare_datatype
  | Declare_datatypes
  | Declare_fun of symbol * 'sort list * 'sort
  | Declare_sort of symbol * int
  | Define_fun
  | Define_fun_rec
  | Define_funs_rec
  | Define_sort (* of (symbol, symbols sort) *)
  | Echo of string
  | Exit
  | Get_assertions
  | Get_assignment
  | Get_info (* of info_flag *)
  | Get_model
  | Get_option of string
  | Get_proof
  | Get_unsat_assumptions
  | Get_unsat_core
  | Get_value of 'term list
  | Pop of int
  | Push of int
  | Reset
  | Reset_assertions
  | Set_info of attribute
  | Set_logic of string
  | Set_option

and symbol = string

and spec_constant =
  | Num of int
  | Dec of float
  | Hex of string
  | Bin of string
  | Str of string

and index =
  | I of int
  | S of symbol

and identifier =
  | Sym of symbol
  | Hole of symbol * index list

and sort =
  | Sort of identifier
  | Sort_comp of identifier * sort list

and attribute_value =
  | Attr_const of spec_constant
  | Attr_sym of symbol

and attribute =
  | Kw of string
  | Kw_val of string * attribute_value

and qual_identifier =
  | Plain of identifier
  | As of identifier * sort

and binding = symbol * term
and sorted_var = symbol * sort

and term =
  | Const of spec_constant
  | Id of qual_identifier
  | App of qual_identifier * term list
  | Let of binding list * term
  | Forall of sorted_var list * term
  | Exists of sorted_var list * term

module Fmt = struct
  open Format

  let pp = fprintf

  let pp_const fmt = function
    | Num x -> pp_print_int fmt x
    | Dec x -> pp_print_float fmt x
    | Hex x | Bin x | Str x -> pp_print_string fmt x

  let pp_index fmt = function
    | I i -> pp_print_int fmt i
    | S i -> pp_print_string fmt i

  let pp_identifier fmt = function
    | Sym id -> pp_print_string fmt id
    | Hole (id, indices) ->
      pp fmt "(_ %s %a)" id
        (pp_print_list ~pp_sep:pp_print_space pp_index)
        indices

  let rec pp_sort fmt = function
    | Sort id -> pp_identifier fmt id
    | Sort_comp (id, sorts) ->
      pp fmt "(%a %a)" pp_identifier id
        (pp_print_list ~pp_sep:pp_print_space pp_sort)
        sorts

  let pp_attribute_value fmt = function
    | Attr_const const -> pp_const fmt const
    | Attr_sym sym -> pp_print_string fmt sym

  let pp_attribute fmt = function
    | Kw kw -> pp_print_string fmt kw
    | Kw_val (kw, v) -> pp fmt "%s %a" kw pp_attribute_value v

  let pp_qual_identifier fmt = function
    | Plain id -> pp_identifier fmt id
    | As _ -> assert false

  let rec pp_term fmt = function
    | Const x -> pp_const fmt x
    | Id x -> pp_qual_identifier fmt x
    | App (id, terms) ->
      pp fmt "(%a@ %a)" pp_qual_identifier id
        (pp_print_list ~pp_sep:pp_print_space pp_term)
        terms
    | Let (binds, term) ->
      pp fmt "(let (%a)@ %a)" pp_bindings binds pp_term term
    | Forall (vars, term) ->
      pp fmt "(forall (%a)@ %a)" pp_vars vars pp_term term
    | Exists (vars, term) ->
      pp fmt "(exists (%a)@ %a)" pp_vars vars pp_term term

  and pp_bindings fmt binds =
    pp_print_list ~pp_sep:pp_print_space
      (fun fmt (symb, term) -> pp fmt "(%s %a)" symb pp_term term)
      fmt binds

  and pp_vars fmt vars =
    pp_print_list ~pp_sep:pp_print_space
      (fun fmt (var, sort) -> pp fmt "(%s %a)" var pp_sort sort)
      fmt vars

  let pp_cmd fmt = function
    | Assert term -> pp fmt "(assert @[<hov 2>%a@])" pp_term term
    | Check_sat -> pp_print_string fmt "(check-sat)"
    | Check_sat_assuming -> pp_print_string fmt "(check-sat-assuming)"
    | Declare_const (x, sort) -> pp fmt "(declare-const %s %a)" x pp_sort sort
    | Declare_datatype -> pp_print_string fmt "(declare-datatype)"
    | Declare_datatypes -> pp_print_string fmt "(declare-datatypes)"
    | Declare_fun (f, args, sort) ->
      pp fmt "(declare-fun %s (%a) %a)" f
        (pp_print_list ~pp_sep:pp_print_space pp_sort)
        args pp_sort sort
    | Declare_sort (sort, n) -> pp fmt "(declare-sort %s %d)" sort n
    | Define_fun -> pp_print_string fmt "(define-fun)"
    | Define_fun_rec -> pp_print_string fmt "(define-fun-rec)"
    | Define_funs_rec -> pp_print_string fmt "(define-funs-rec)"
    | Define_sort -> pp_print_string fmt "(define-sort)"
    | Echo str -> pp fmt "(echo %s)" str
    | Exit -> pp_print_string fmt "(exit)"
    | Get_assertions -> pp_print_string fmt "(get-assertions)"
    | Get_assignment -> pp_print_string fmt "(get-assignment)"
    | Get_info -> pp_print_string fmt "(get-info)"
    | Get_model -> pp_print_string fmt "(get-model)"
    | Get_option x -> pp fmt "(get-option %s)" x
    | Get_proof -> pp_print_string fmt "(get-proof)"
    | Get_unsat_assumptions -> pp_print_string fmt "(get-unsat-assumptions)"
    | Get_unsat_core -> pp_print_string fmt "(get-unsat-core)"
    | Get_value _ -> pp_print_string fmt "(get-value)"
    | Pop n -> pp fmt "(pop %d)" n
    | Push n -> pp fmt "(push %d)" n
    | Reset -> pp_print_string fmt "(reset)"
    | Reset_assertions -> pp_print_string fmt "(reset-assertions)"
    | Set_info x -> pp fmt "(set-info %a)" pp_attribute x
    | Set_logic x -> pp fmt "(set-logic %s)" x
    | Set_option -> pp_print_string fmt "(set-option)"

  let pp_script fmt v = pp_print_list ~pp_sep:pp_print_newline pp_cmd fmt v
end
