module Make (M : sig
  type t
  type tok

  exception Error
  exception SyntaxError of string

  val rule : (Lexing.lexbuf -> tok) -> Lexing.lexbuf -> t
  val token : Lexing.lexbuf -> tok
end) =
struct
  let pp_pos fmt (lexbuf : Lexing.lexbuf) =
    let pos = lexbuf.lex_curr_p in
    Format.fprintf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)

  let from_lexbuf ?file lexbuf =
    Format.printf "Parsing%a ...@."
      (Format.pp_print_option (fun fmt s -> Format.fprintf fmt " %s" s))
      file;
    try Ok (M.rule M.token lexbuf) with
    | M.SyntaxError msg ->
      Format.kasprintf Result.error "%a: %s@\n" pp_pos lexbuf msg
    | M.Error ->
      Format.kasprintf Result.error "%a: syntax error@\n" pp_pos lexbuf

  let from_file filename =
    In_channel.with_open_text filename (fun chan ->
        let lexbuf = Lexing.from_channel chan in
        from_lexbuf ~file:filename lexbuf )

  let from_string ?file contents =
    from_lexbuf ?file (Lexing.from_string contents)
end

module Script = Make (struct
  type t = Ast.t list
  type tok = Parser.token

  exception Error = Parser.Error
  exception SyntaxError = Lexer.SyntaxError

  let rule = Parser.script
  let token = Lexer.token
end)

module Smtlib = Make (struct
  type t = Smtlib.script
  type tok = Smtlib_parser.token

  exception Error = Smtlib_parser.Error
  exception SyntaxError = Smtlib_lexer.SyntaxError

  let rule = Smtlib_parser.script
  let token = Smtlib_lexer.token
end)
