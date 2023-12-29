{
open Lexing
open Smtlib_parser

exception SyntaxError of string

let keywords =
  let tbl = Hashtbl.create 16 in
  Array.iter
    (fun (k, v) -> Hashtbl.add tbl k v)
    [| (":sorts", SORTS)
     ; (":funs", FUNS)
     ; (":sorts-description", SORTS_DESCRIPTION)
     ; (":funs-description", FUNS_DESCRIPTION)
     ; (":definition", DEFINITION)
     ; (":values", VALUES)
     ; (":notes", NOTES)
     ; (":theories", THEORIES)
     ; (":language", LANGUAGE)
     ; (":extensions", EXTENSIONS)
    |];
  tbl

let reserved =
  let tbl = Hashtbl.create 64 in
  Array.iter
    (fun (k, v) -> Hashtbl.add tbl k v)
    [|
      (* ("BINARY", BINARY) *)
      ("DECIMAL", DECIMAL)
     (* ; ("HEXADECIMAL", HEXADECIMAL) *)
     ; ("NUMERAL", NUMERAL)
     ; ("STRING", STRING)
     ; ("_", HOLE)
     ; ("!", ANNOT)
     ; ("as", AS)
     ; ("let", LET)
     ; ("exists", EXISTS)
     ; ("forall", FORALL)
     ; ("match", MATCH)
     ; ("par", PAR)
     ; ("theory", THEORY)
     ; ("logic", LOGIC)
     ; ("assert", ASSERT)
     ; ("check-sat", CHECK_SAT)
     ; ("check-sat_assuming", CHECK_SAT_ASSUMING)
     ; ("declare-const", DECLARE_CONST)
     ; ("declare-datatype", DECLARE_DATATYPE)
     ; ("declare-datatypes", DECLARE_DATATYPES)
     ; ("declare-fun", DECLARE_FUN)
     ; ("declare-sort", DECLARE_SORT)
     ; ("define-fun", DEFINE_FUN)
     ; ("define-fun-rec", DEFINE_FUN_REC)
     ; ("define-funs-rec", DEFINE_FUN_REC)
     ; ("define-sort", DEFINE_SORT)
     ; ("echo", ECHO)
     ; ("exit", EXIT)
     ; ("get-assertions", GET_ASSERTIONS)
     ; ("get-assignment", GET_ASSIGNMENT)
     ; ("get-info", GET_INFO)
     ; ("get-model", GET_MODEL)
     ; ("get-option", GET_OPTION)
     ; ("get-proof", GET_PROOF)
     ; ("get-unsat-assumptions", GET_UNSAT_ASSUMPTIONS)
     ; ("get-unsat-core", GET_UNSAT_CORE)
     ; ("get-value", GET_VALUE)
     ; ("pop", POP)
     ; ("push", PUSH)
     ; ("reset", RESET)
     ; ("reset-assertions", RESET_ASSERTIONS)
     ; ("set-info", SET_INFO)
     ; ("set-logic", SET_LOGIC)
     ; ("set-option", SET_OPTION)
    |];
  tbl

let error msg = raise (SyntaxError msg)
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let numeral = '0' | [ '1'-'9' ] digit*
let decimal = numeral '.' '0'* numeral
let hexadecimal = "#x" (['a'-'f' 'A'-'F'] | digit)+
let binary = "#b" ('0' | '1')+
let bool = "true" | "false"

let operator = ['~''!''@''$''%''^''&''*''_''-''+''=''<''>''.''?''/']
let simple_symbol = (letter | operator) (letter | digit | operator)*
let symbol = simple_symbol | '|' simple_symbol '|'
let keyword = ':' simple_symbol
(* TODO: Quoted symbols: |symbol| *)

rule token = parse
  | '(' { LPAR }
  | ')' { RPAR }

  | numeral as x { NUM (int_of_string x) }
  | decimal as x { DEC (float_of_string x) }
  | hexadecimal as x { HEX x }
  | binary as x { BIN x }
  | '"' { string (Buffer.create 17) lexbuf }
  | keyword as x { try Hashtbl.find keywords x with Not_found -> KEYWORD x }
  | symbol as x { try Hashtbl.find reserved x with Not_found -> SYMBOL x }

  | ';' { comment lexbuf }
  | white { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }
  | eof { EOF }

  | _ { error ("Unexpected char: " ^ Lexing.lexeme lexbuf) }

and comment = parse
  | newline { new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }

and string buf = parse
  | '"' { STR (Buffer.contents buf) }
  | '"' '"' { Buffer.add_char buf '"'; string buf lexbuf }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }
  | eof { error "nonterminated string" }
  | _ { error ("illegal string char: " ^ Lexing.lexeme lexbuf) }
