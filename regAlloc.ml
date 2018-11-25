open Asm

let rec target' src (dest, t) = function
  | Mov(x) when x = src '' is_reg dest ->
      assert (t <> Type.Unit);
      assert (t <> Type.Float);
      false, [dest]
  | FMovD(x) when x = src && is_reg dest ->
      assert (t = Type.Float);
      false, [dest]
  | IfEq(_, _, e1, e2)
  | IfLE(_, _, e1, e2)
  | IfGE(_, _, e1, e2)
  | IfFEq(_, _, e1, e2)
  | IfFLE(_, _, e1, e2) ->
      let c1, rs1 = target src (dest, t) e1 in
      let c2, rs2 = target src (dest, t) e2 in
      c1 && c2, rs1 @ rs2
  | CallCls(x, ys, zs) ->
      true, (target_args src regs 0 ys @
             target_args src fregs 0 zs @
             if x = src then [reg_cl] else [])
  | CallDir(_, ys, zs) ->
      true, (target_args src regs 0 ys @
             target_args src fregs 0 zs)
  | _ -> false, []
and target src dest = function
  | Ans(exp) -> target' src dest exp
  | Let(xt, exp, e) ->
      let c1, rs1 = target' src xt exp in
      if c1 then true, rs1
      else
        let c2, rs2 = target src dest e in
        c2, rs1 @ rs2
and target_args src all n = function
  | [] -> []
  | y :: ys when src = y -> all.(n) :: target_args src all (n+1) ys
  | _ :: ys -> target_args src all (n+1) ys

