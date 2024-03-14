open Encoding
open Expr
module Z3 = Solver.Z3_batch

let () =
  let module I8 = Bitv.I8 in
  let solver = Z3.create ~logic:QF_BVFP () in
  let x = I8.sym "x" in
  assert (Z3.check solver [ I8.(x > v 0) ]);
  let v = Z3.get_value solver x in
  assert (v >= I8.v 1)
