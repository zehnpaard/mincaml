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

let reg g dest cont regenv = function
  | Ans(exp) -> g'_and_restore dest cont regenv exp
  | Let((x, t) as xt, exp, e) ->
      assert (not (M.mem x regenv));
      let cont' = concat e dest cont in
      let (e1', regenv1) = g'_and_restore xt cont' regenv exp in
      (match alloc_dest cont' regenv1 x t with
         | Spill(y) ->
             let r = M.find y regenv1 in
             let (e2', regenv2) = g dest cont (add x r (M.remove y regenv1)) e in
             let save =
               try Save(M.find y regenv, y)
               with Not_found -> Nop
             in
             (seq(save, concat e1' (r, t) e2'), regenv2)
         | Alloc(r) ->
             let (e2', regenv2) = g dest cont (add x r regenv1) e in
             (concat e1' (r, t) e2', regenv2))
and g'_and_restore dest cont regenv exp =
  try g' dest cont regenv exp
  with NoReg(x, t) -> g dest cont regenv (Let((x, t), Restore(x), Ans(exp)))
and g' dest cont regenv = function
  | Nop
  | Set _
  | SetL _
  | Comment _
  | Restore _ as exp -> (Ans(exp), regenv)
  | Mov(x) -> (Ans(Mov(find x Type.Int regenv)), regenv)
  | Neg(x) -> (Ans(Neg(find x Type.Int regenv)), regenv)
  | Add(x, y) -> (Ans(Add(find x Type.Int regenv, find' y regenv)), regenv)
  | Sub(x, y) -> (Ans(Sub(find x Type.Int regenv, find' y regenv)), regenv)
  | SLL(x, y) -> (Ans(SLL(find x Type.Int regenv, find' y regenv)), regenv)
  | Ld(x, y) -> (Ans(Ld(find x Type.Int regenv, find' y regenv)), regenv)
  | St(x, y, z) ->
      let x' = find x Type.Int regenv in
      let y' = find y Type.Int regenv in
      let z' = find' z regenv in
      (Ans(St(x', y', z')), regenv)
  | FMovD(x) -> (Ans(FMovD(find x Type.Float regenv)), regenv)
  | FNegD(x) -> (Ans(FNegD(find x Type.Float regenv)), regenv)
  | FAddD(x, y) ->
      let x' = find x Type.Float regenv in
      let y' = find y Type.Float regenv in
      (Ans(FAddD(x', y')), regenv)
  | FSubD(x, y) ->
      let x' = find x Type.Float regenv in
      let y' = find y Type.Float regenv in
      (Ans(FSubD(x', y')), regenv)
  | FMulD(x, y) ->
      let x' = find x Type.Float regenv in
      let y' = find y Type.Float regenv in
      (Ans(FMulD(x', y')), regenv)
  | FDivD(x, y) ->
      let x' = find x Type.Float regenv in
      let y' = find y Type.Float regenv in
      (Ans(FDivD(x', y')), regenv)
  | LdDF(x, y) -> (Ans(LdDF(find x Type.Int regenv, find' y regenv)), regenv) (* really? *)
  | StDF(x, y, z) ->
      let x' = find x Type.Float regenv in
      let y' = find y Type.Float regenv in
      let z' = find' z regenv in
      (Ans(StDF(x', y', z')), regenv)
  | IfEq(x, y, e1, e2) as exp ->
      let f e1' e2' = IfEq(find x Type.Int regenv, find' y regenv, e1', e2') in
      g'_if dest cont regenv exp f e1 e2
  | IfLE(x, y, e1, e2) as exp ->
      let f e1' e2' = IfLE(find x Type.Int regenv, find' y regenv, e1', e2') in
      g'_if dest cont regenv exp f e1 e2
  | IfGE(x, y, e1, e2) as exp ->
      let f e1' e2' = IfGE(find x Type.Int regenv, find' y regenv, e1', e2') in
      g'_if dest cont regenv exp f e1 e2
  | IfFEq(x, y, e1, e2) as exp ->
      let f e1' e2' = IfFEq(find x Type.Float regenv, find y Type.Float regenv, e1', e2') in
      g'_if dest cont regenv exp f e1 e2
  | IfFLE(x, y, e1, e2) as exp ->
      let f e1' e2' = IfFLE(find x Type.Float regenv, find y Type.Float regenv, e1', e2') in
      g'_if dest cont regenv exp f e1 e2
  | CallCls(x, ys, zs) as exp ->
      let f ys zs = CallCls(find x Type.Int regenv, ys, zs) in
      g'_call dest cont regenv exp f ys zs
  | CallCls(l, ys, zs) as exp ->
      let f ys zs = CallDir(l, ys, zs) in
      g'_call dest cont regenv exp f ys zs
  | Save(x, y) -> assert false
and g'_if dest cont regenv exp constr e1 e2 =
  let (e1', regenv1) = g dest cont regenv e1 in
  let (e2', regenv2) = g dest cont regenv e2 in
  let f1 regenv x =
    try
      if is_reg x then regenv
      else
        let r1 = M.find x regenv1 in
        let r2 = M.find x regenv2 in
        if r1 <> r2 then regenv
        else M.add x r1 regenv
    with Not_found -> regenv
  in
  let regenv' = List.fold_left f1 M.empty (fv cont) in
  let f2 e 2 =
    if x = fst dest || not (M.mem x regenv) then e
    else seq(Save(M.find x regenv, x), e)
  in
  (List.fold_left f2 Ans(constr e1' e2') (fv cont), regenv')
and g'_call dest cont regenv exp constr ys zs =
  let f e x =
    if x = fst dest || not (M.mem x regenv) then e
    else seq(Save(M.find x regenv, x), e)
  in
  let a =
    Ans(constr
          (List.map (fun y -> find y Type.Int regenv) ys)
          (List.map (fun z -> find z Type.Float regenv) zs))
  in
  (List.fold_left f a (fv cont), M.empty)
