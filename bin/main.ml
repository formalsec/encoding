open Encoding
open Cmdliner
open Syntax.Result
module Z3_batch = Solver.Incremental (Z3_mappings)
module Interpret = Interpret_smt.Make (Z3_batch)
module Smtlib_parser = Parse.Smtlib

let parse_file = function
  | "-" -> In_channel.input_all stdin |> Smtlib_parser.from_string
  | filename -> Smtlib_parser.from_file filename

let fmt file =
  match Smtlib_parser.from_file file with
  | Ok script ->
    Format.printf "%a@." Smtlib.Fmt.pp_script script;
    0
  | Error msg ->
    Format.eprintf "error: %s@." msg;
    1

let display_stats print_stats (state : Interpret.exec_state) =
  if print_stats then
    Format.printf "%a@." Z3_batch.pp_statistics state.solver

let parse_then_simplify_then_run stats file =
  let* script = parse_file file in
  let* script = Rewrite.script script in
  let+ state = Interpret.main script in
  display_stats stats state

let run files stats =
  let result = list_iter ~f:(parse_then_simplify_then_run stats) files in
  match result with
  | Ok () -> 0
  | Error msg ->
    Format.eprintf "error: %s@." msg;
    1

let help = [ `S Manpage.s_common_options ]
let sdocs = Manpage.s_common_options

let fmt_cmd =
  let file =
    let doc = "source file" in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc)
  in
  let doc = "format smt-lib scripts" in
  let info = Cmd.info "fmt" ~doc ~sdocs in
  Cmd.v info Term.(const fmt $ file)

let run_cmd =
  let files =
    let doc = "source files" in
    Arg.(value & pos_all non_dir_file [] & info [] ~doc)
  in
  let stats =
    let doc = "print solver statistics" in
    Arg.(value & flag & info [ "stats" ] ~doc)
  in
  let doc = "interpret smt-lib files" in
  let info = Cmd.info "run" ~doc ~sdocs in
  Cmd.v info Term.(const run $ files $ stats)

let cli =
  let doc = "a toy smt-lib interpreter" in
  let man = help in
  let info = Cmd.info "smtml" ~version:"%%VERSION%%" ~doc ~sdocs ~man in
  Cmd.group info [ run_cmd; fmt_cmd ]

let () = exit @@ Cmd.eval' cli
