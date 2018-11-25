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

type alloc_result =
  | Alloc of Id.t
  | Spill of Id.t
let rec alloc dest cont regenv x t =
  assert (not (M.mem x regenv));
  let all =
    match t with
      | Type.Unit -> ["%g0"]
      | Type.Float -> allfregs
      | _ -> allregs
  in
  if all = ["%g0"] then 
    Alloc("%g0")
  else if is_reg x then 
    Alloc(x)
  else
    let free = fv cont in
    try
      let (c, prefer) = target x dest cont in
      let f lv y =
        if is_reg y then 
          S.add y lv
        else
          try S.add (M.find y regenv) lv
          with Not_found -> lv
      in
      let live = List.fold_left f S.empty free in
      let r = List.find (fun r -> not (S.mem r live)) (prefer @ all) in
      Alloc(r)
    with Not_found ->
      let f y = 
        not (is_reg y) && 
        (try List.mem (M.find y regenv) all
         with Not_found -> false)
      in
      let y = List.find f (List.rev free) in
      Spill(y)

let add x r regenv =
  if is_reg x then
    (assert (x = r); regenv)
  else
    M.add x r regenv

exception NoReg of Id.t * Type.t
let find x t regenv =
  if is_reg x then x
  else
    try M.find x regenv
    with Not_found -> raise (NoReg(x, t))
let find' x' regenv =
  match x' with
    | V(x) -> V(find x Type.Int regenv)
    | c -> c
