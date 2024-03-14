open Encoding

let () =
  let open Expr.Bitv in
  let x = I32.sym "x" in
  let y = I32.sym "y" in
  let e1 = I32.(x > y) in
  let e2 = I32.(y < x) in
  assert (e1 == e2)
