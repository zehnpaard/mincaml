open Asm

let rec target' src (dest, t) = function
  | Mov(x) when x = src && is_reg dest ->
      assert (t <> Type.Unit);
      false, [dest]
  | CallCls(x, ys, zs) ->
      true, (target_args src regs 0 ys @
             if x = src then [reg_cl] else [])
  | CallDir(_, ys, zs) ->
      true, target_args src regs 0 ys
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

let rec g dest cont regenv = function
  | Ans(exp) -> g'_and_restore dest cont regenv exp
  | Let((x, t) as xt, exp, e) ->
      assert (not (M.mem x regenv));
      let cont' = concat e dest cont in
      let (e1', regenv1) = g'_and_restore xt cont' regenv exp in
      (match alloc dest cont' regenv1 x t with
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
  | Sub(x, y) -> (Ans(Sub(find x Type.Int regenv, find' y regenv)), regenv)
  | SLL(x, y) -> (Ans(SLL(find x Type.Int regenv, find' y regenv)), regenv)
  | Ld(x, y) -> (Ans(Ld(find x Type.Int regenv, find' y regenv)), regenv)
  | St(x, y, z) ->
      let x' = find x Type.Int regenv in
      let y' = find y Type.Int regenv in
      let z' = find' z regenv in
      (Ans(St(x', y', z')), regenv)
  | CallCls(x, ys, zs) as exp ->
      let f ys zs = CallCls(find x Type.Int regenv, ys, zs) in
      g'_call dest cont regenv exp f ys zs
  | CallCls(l, ys, zs) as exp ->
      let f ys zs = CallDir(l, ys, zs) in
      g'_call dest cont regenv exp f ys zs
  | Save(x, y) -> assert false
and g'_call dest cont regenv exp constr ys zs =
  let f e x =
    if x = fst dest || not (M.mem x regenv) then e
    else seq(Save(M.find x regenv, x), e)
  in
  let a =
    Ans(constr (List.map (fun y -> find y Type.Int regenv) ys))
  in
  (List.fold_left f a (fv cont), M.empty)

let h { name = Id.L(x); args = ys; fargs = zs; body = e; ret = t } =
  let regenv = M.add x reg_cl M.empty in
  let f (i, arg_regs, regenv) y =
    let r = regs.(i) in
    (i + 1, arg_regs @ [r], (assert (not (is_reg y)); M.add y r regenv))
  in
  let (i, arg_regs, regenv) = List.fold_left f (0, [], regenv) ys in
  let a = match t with
    | Type.Unit -> Id.gentmp Type.Unit
    | _ -> regs.(0)
  in
  let (e', regenv') = g (a, t) (Ans(Mov(a))) regenv e in
  { name = Id.L(x); args = arg_regs; body = e'; ret = t }

let f (Prog(data, fundefs, e)) =
  let fundefs' = List.map h fundefs in
  let e', regenv' = g (Id.gentmp Type.Unit, Type.Unit) (Ans(Nop)) M.empty e in
  Prog(data, fundefs', e')
