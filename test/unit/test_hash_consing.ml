open Encoding
open Expr

let () =
  let module I32 = Expr.Bitv.I32 in
  assert (I32.sym "x" == I32.sym "x");
  assert (I32.sym "x" != I32.sym "y");
  let left_a = I32.sym "x" in
  let right_a = I32.sym "y" in
  let left_b = I32.sym "x" in
  let right_b = I32.sym "y" in
  let a = Expr.Binop (Ty.Add, left_a, right_a) @: Ty.Ty_int in
  let b = Expr.Binop (Ty.Add, left_b, right_b) @: Ty.Ty_int in
  assert (a == b);
  (*
     There should be only 3 elements in the hashcons table:
       1. x
       2. y
       3. x + y
  *)
  assert (Expr.H.length () == 3)
