open Encoding
module Batch = Solver.Batch (Z3_mappings)
module Interpret = Interpret.Make (Batch)

let parse = Parse.Script.from_string

let%test_unit _ =
  let script =
    {|
    (declare-fun x real)
    (declare-fun y real)
    (assert (real.eq y (real.mul x x)))
    (assert (real.eq y 2.0))
    |}
  in
  ignore @@ parse script
