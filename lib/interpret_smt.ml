open Syntax.Result

module Make (Solver : Solver_intf.S) = struct
  open Smtlib

  type exec_state =
    { cmds : (Expr.t, Ty.t) Smtlib.command list
    ; ty_env : (string, Ty.t) Hashtbl.t
    ; pc : Expr.t list
    ; solver : Solver.t
    }

  let init_state cmds =
    let params = Params.(default () & (Model, false)) in
    { cmds
    ; ty_env = Hashtbl.create 0
    ; solver = Solver.create ~params ()
    ; pc = []
    }

  let exec_cmd cmd (state : exec_state) : (exec_state, string) Result.t =
    let { solver; pc; _ } = state in
    let st pc = Ok { state with pc } in
    match cmd with
    | Assert t ->
      Solver.add solver [ t ];
      st (t :: pc)
    | Check_sat ->
      if Solver.check solver [] then Format.printf "sat\n"
      else Format.printf "unsat\n";
      st pc
    | Check_sat_assuming -> assert false
    | Declare_const (_sym, _ty) -> st pc
    | Echo msg ->
      Format.printf "%s@\n" msg;
      st pc
    | Exit -> Ok { state with cmds = [] }
    | Get_model ->
      assert (Solver.check solver []);
      let model = Solver.model solver in
      Format.printf "%a" Model.pp (Option.get model);
      st pc
    | Set_info _ -> st pc
    | _ -> Error (Format.asprintf {|interpreter: Unsupported command "TODO".|})

  let rec loop (state : exec_state) : (exec_state, string) Result.t =
    match state.cmds with
    | [] -> Ok state
    | cmd :: cmds ->
      let* state = exec_cmd cmd { state with cmds } in
      loop state

  let main ?state (cmds : (Expr.t, Ty.t) Smtlib.command list) :
    (exec_state, string) Result.t =
    let st =
      match state with
      | None -> init_state cmds
      | Some st ->
        Solver.reset st.solver;
        { st with cmds; pc = [] }
    in
    loop st
end
