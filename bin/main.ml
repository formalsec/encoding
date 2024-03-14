open Encoding

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename ->
    let chan = open_in filename in
    Fun.protect
      ~finally:(fun () -> close_in chan)
      (fun () -> In_channel.input_all chan)

let parse_file file = get_contents file |> Parse.from_string

type prover =
  | Z3_prover
  | Colibri2_prover

type solver =
  | Bat
  | Bat_cache
  | Incr

let prover_conv =
  Cmdliner.Arg.enum
    [ ("z3", Z3_prover)
    ; ("Z3", Z3_prover)
    ; ("c2", Colibri2_prover)
    ; ("colibri2", Colibri2_prover)
    ; ("Colibri2", Colibri2_prover)
    ]

let solver_conv =
  Cmdliner.Arg.enum
    [ ("batch", Bat); ("batch-cache", Bat_cache); ("incremental", Incr) ]

let parse_cmdline =
  let aux files prover solver debug print_statistics =
    let module Mappings =
      ( val match prover with
            | Z3_prover -> (module Z3_mappings)
            | Colibri2_prover ->
              Log.err "Please install Colibri2 and use 'smtml2'"
          : Mappings_intf.S )
    in
    Mappings.set_debug debug;
    let module Solver =
      ( val match solver with
            | Bat -> (module Solver.Batch (Mappings))
            | Bat_cache -> (module Solver.Batch_cached (Mappings))
            | Incr -> (module Solver.Incremental (Mappings))
          : Solver_intf.S )
    in
    let module Interpret = Interpret.Make (Solver) in
    let state =
      match files with
      | [] ->
        let ast = parse_file "-" in
        Some (Interpret.start ast)
      | _ ->
        List.fold_left
          (fun state file ->
            let ast = Parse.from_file ~filename:file in
            Some (Interpret.start ?state ast) )
          None files
    in
    if print_statistics then begin
      let state = Option.get state in
      let stats : Gc.stat = Gc.stat () in
      Format.eprintf
        "@[<v 2>(statistics @\n\
         (major-words %f)@\n\
         (solver-time %f)@\n\
         (solver-calls %d)@\n\
         @[<v 2>(solver-misc @\n\
         %a@])@])@\n"
        stats.major_words !Solver.solver_time !Solver.solver_count
        Solver.pp_statistics state.solver
    end
  in
  let open Cmdliner in
  let files =
    Arg.(value & pos_all string [] & info [] ~docv:"files" ~doc:"files to read")
  and prover =
    Arg.(
      value & opt prover_conv Z3_prover
      & info [ "p"; "prover" ] ~doc:"SMT solver to use" )
  and solver =
    Arg.(
      value & opt solver_conv Bat
      & info [ "solver" ] ~doc:"Use the SMT solver mode" )
  and debug =
    Arg.(value & flag & info [ "debug" ] ~doc:"Print debugging messages")
  and print_statistics =
    Arg.(value & flag & info [ "st" ] ~doc:"Print statistics")
  in
  Cmd.v
    (Cmd.info "smtml" ~version:"%%VERSION%%")
    Term.(const aux $ files $ prover $ solver $ debug $ print_statistics)

let () =
  match Cmdliner.Cmd.eval_value parse_cmdline with
  | Error (`Parse | `Term | `Exn) -> exit 2
  | Ok (`Ok () | `Version | `Help) -> ()
