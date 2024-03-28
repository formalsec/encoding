open Encoding
open Bitwuzla_cxx
include Mappings_intf

module Impl = struct
  type ty = Sort.t

  type term = Term.t

  type interp = Term.t

  type model = unit (* TODO *)

  type solver = Solver.t

  type handle = unit

  type optimizer = unit (* Not supported? *)

  type cont = unit

  type 'a t = 'a

  let make_cont () = ()

  module Cont = struct
    let return v = v [@@inline]

    let bind v f = f v [@@inline]

    let ( let* ) v f = bind v f [@@inline]

    let map v f = f v [@@inline]

    let ( let+ ) v f = map v f [@@inline]

    let run v () = v [@@inline]
  end

  let true_ = mk_true ()

  let false_ = mk_false ()

  let int _ = assert false

  let real _ = assert false

  let const symbol ty = mk_const ~symbol ty

  let not_ t = mk_term1 Kind.Not t

  let and_ t1 t2 = mk_term2 Kind.And t1 t2

  let or_ t1 t2 = mk_term2 Kind.Or t1 t2

  let xor t1 t2 = mk_term2 Kind.Xor t1 t2

  let eq t1 t2 = mk_term2 Kind.Equal t1 t2

  let distinct ts = mk_term Kind.Distinct (Array.of_list ts)

  let ite cond t1 t2 = mk_term3 Kind.Ite cond t1 t2

  module Types = struct
    let int = Obj.magic 0 [@@alert not_implemented "Not supported"]

    let real = Obj.magic 0

    let bool = Obj.magic 0

    let string = Obj.magic 0

    let bitv bitwidth = mk_bv_sort bitwidth

    let float ebits sbits = mk_fp_sort ebits sbits

    let ty t = Term.sort t

    let to_ety _ = assert false
  end

  module Interp = struct
    let to_int _ = assert false

    let to_real _ = assert false

    let to_bool t = Term.value Term.Bool t

    let to_string _ = assert false

    let to_bitv t _bitwidth = Z.to_int64 @@ Term.value Term.Z t

    let to_float _t _ebits _sbits = assert false
  end

  module Int = struct
    let neg _ = assert false

    let to_real _ = assert false

    let add _ = assert false

    let sub _ = assert false

    let mul _ = assert false

    let div _ = assert false

    let rem _ = assert false

    let pow _ = assert false

    let lt _ = assert false

    let le _ = assert false

    let gt _ = assert false

    let ge _ = assert false
  end

  module Real = struct
    let neg _ = assert false

    let to_int _ = assert false

    let add _ = assert false

    let sub _ = assert false

    let mul _ = assert false

    let div _ = assert false

    let pow _ = assert false

    let lt _ = assert false

    let le _ = assert false

    let gt _ = assert false

    let ge _ = assert false
  end

  module String = struct
    let v _ = assert false

    let length _ = assert false

    let to_code _ = assert false

    let of_code _ = assert false

    let at _ = assert false

    let concat _ = assert false

    let sub _ ~pos:_ ~len:_ = assert false
  end

  module Bitv = struct
    let v v_str bitwidth = mk_bv_value (Types.bitv bitwidth) v_str 10

    let neg t = mk_term1 Kind.Bv_neg t

    let lognot t = mk_term1 Kind.Bv_not t

    let add t1 t2 = mk_term2 Kind.Bv_add t1 t2

    let sub t1 t2 = mk_term2 Kind.Bv_sub t1 t2

    let mul t1 t2 = mk_term2 Kind.Bv_mul t1 t2

    let div t1 t2 = mk_term2 Kind.Bv_sdiv t1 t2

    let div_u t1 t2 = mk_term2 Kind.Bv_udiv t1 t2

    let logor t1 t2 = mk_term2 Kind.Bv_or t1 t2

    let logand t1 t2 = mk_term2 Kind.Bv_and t1 t2

    let logxor t1 t2 = mk_term2 Kind.Bv_xor t1 t2

    let shl t1 t2 = mk_term2 Kind.Bv_shl t1 t2

    let ashr t1 t2 = mk_term2 Kind.Bv_ashr t1 t2

    let lshr t1 t2 = mk_term2 Kind.Bv_shr t1 t2

    let rem t1 t2 = mk_term2 Kind.Bv_srem t1 t2

    let rem_u t1 t2 = mk_term2 Kind.Bv_urem t1 t2

    let rotate_left t1 t2 = mk_term2 Kind.Bv_rol t1 t2

    let rotate_right t1 t2 = mk_term2 Kind.Bv_ror t1 t2

    let lt t1 t2 = mk_term2 Kind.Bv_slt t1 t2

    let lt_u t1 t2 = mk_term2 Kind.Bv_ult t1 t2

    let le t1 t2 = mk_term2 Kind.Bv_sle t1 t2

    let le_u t1 t2 = mk_term2 Kind.Bv_ule t1 t2

    let gt t1 t2 = mk_term2 Kind.Bv_sgt t1 t2

    let gt_u t1 t2 = mk_term2 Kind.Bv_ugt t1 t2

    let ge t1 t2 = mk_term2 Kind.Bv_sge t1 t2

    let ge_u t1 t2 = mk_term2 Kind.Bv_uge t1 t2

    let concat t1 t2 = mk_term2 Kind.Bv_concat t1 t2

    let extract x ~high ~low = mk_term1_indexed2 Kind.Bv_extract x high low

    let zero_extend n t = mk_term1_indexed1 Kind.Bv_zero_extend t n

    let sign_extend n t = mk_term1_indexed1 Kind.Bv_sign_extend t n
  end

  module Float = struct
    module Rounding_mode = struct
      let rne = mk_rm_value RoundingMode.Rne

      let rna = mk_rm_value RoundingMode.Rna

      let rtp = mk_rm_value RoundingMode.Rtp

      let rtn = mk_rm_value RoundingMode.Rtn

      let rtz = mk_rm_value RoundingMode.Rtz
    end

    let v _ = assert false

    let neg _ = assert false

    let abs _ = assert false

    let sqrt ~rm:_ _ = assert false

    let is_nan _ = assert false

    let round_to_integral ~rm:_ _ = assert false

    let add ~rm:_ _ = assert false

    let sub ~rm:_ _ = assert false

    let mul ~rm:_ _ = assert false

    let div ~rm:_ _ = assert false

    let min _ = assert false

    let max _ = assert false

    let rem _ = assert false

    let eq _ = assert false

    let lt _ = assert false

    let le _ = assert false

    let gt _ = assert false

    let ge _ = assert false

    let to_fp _ _ ~rm:_ _ = assert false

    let sbv_to_fp _ _ ~rm:_ _ = assert false

    let ubv_to_fp _ _ ~rm:_ _ = assert false

    let to_ubv _ ~rm:_ _ = assert false

    let to_sbv _ ~rm:_ _ = assert false

    let of_ieee_bv _ = assert false

    let to_ieee_bv _ = assert false
  end

  module Model = struct
    let get_symbols _ = assert false

    let eval ?completion:_ _ = assert false
  end

  module Solver = struct
    let make ?params:_ ?logic:_ () = Solver.create (Options.default ())

    let clone _ = assert false

    let push solver = Solver.push solver 1

    let pop solver n = Solver.pop solver n

    let reset _ = assert false

    let add solver ts = List.iter (Solver.assert_formula solver) ts

    let check solver ~assumptions =
      let assumptions = Array.of_list assumptions in
      match Solver.check_sat ~assumptions solver with
      | Result.Sat -> `Sat
      | Result.Unsat -> `Unsat
      | Result.Unknown -> `Unknown

    let model _ = assert false

    let add_simplifier solver = solver

    let interrupt _ = ()

    let pp_statistics fmt solver = Solver.pp_statistics fmt solver
  end

  module Optimizer = struct
    let make _ = assert false

    let push _ = assert false

    let pop _ = assert false

    let add _ = assert false

    let check _ = assert false

    let model _ = assert false

    let maximize _ = assert false

    let minimize _ = assert false

    let interrupt _ = assert false

    let pp_statistics _ = assert false
  end
end

module Impl' : M = Impl

include Mappings.Make (Impl)
