include Cache_intf

module Tbl = Hashtbl.Make (struct
  type t = Expr.t list

  let equal l1 l2 =
    List.compare_lengths l1 l2 = 0 && List.for_all2 Expr.equal l1 l2

  let hash es = List.fold_left (fun acc e -> Expr.hash e + acc) 0 es
end)
