module Script : sig
  val from_file : string -> (Ast.t list, string) Result.t
  val from_string : ?file:string -> string -> (Ast.t list, string) Result.t
end

module Smtlib : sig
  val from_file : string -> (Smtlib.script, string) Result.t
  val from_string : ?file:string -> string -> (Smtlib.script, string) Result.t
end
