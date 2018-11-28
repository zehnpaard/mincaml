open Asm

let rec target' src (dest, t) = function
  | Mov(x) when x = src && is_reg dest -> [dest]
  | _ -> []
and target src dest = function
  | Ans(exp) -> target' src dest exp
  | Let(xt, exp, e) ->
      let rs1 = target' src xt exp in
      let rs2 = target src dest e in
      rs1 @ rs2

type alloc_result =
  | Alloc of Id.t
  | Spill of Id.t
let rec alloc dest cont regenv x t =
  assert (not (M.mem x regenv));
  if allregs = ["%g0"] then 
    Alloc("%g0")
  else if is_reg x then 
    Alloc(x)
  else
    let free = fv cont in
    try
      let prefer = target x dest cont in
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
  try g' regenv exp
  with NoReg(x, t) -> g dest cont regenv (Let((x, t), Restore(x), Ans(exp)))
and g' regenv = function
  | Nop
  | Set _
  | Comment _
  | Restore _ as exp -> (Ans(exp), regenv)
  | Mov(x) -> (Ans(Mov(find x Type.Int regenv)), regenv)
  | Sub(x, y) -> (Ans(Sub(find x Type.Int regenv, find' y regenv)), regenv)
  | Save(x, y) -> assert false

let f (Prog(e)) =
  let e', regenv' = g (Id.gentmp Type.Int, Type.Int) (Ans(Nop)) M.empty e in
  Prog(e')
