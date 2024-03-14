module type S = sig
  include Hashtbl.S with type key = Expr.t list
end

module type Intf = sig
  module type S = S
  module Tbl : S
end
