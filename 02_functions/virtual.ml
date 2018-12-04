open Asm

let data = ref []

let classify xts ini addi =
  let f acc (x, t) = match t with
    | Type.Unit -> acc
    | _ -> addi acc x t
  in
  List.fold_left f ini xts

let separate xts =
  classify
    xts
    []
    (fun int x _ -> int @ [x])

let expand xts ini addi =
  classify
    xts
    ini
    (fun (offset, acc) x t ->
       (offset + 4, addi x t offset acc))

let rec g env = function
  | Closure.Unit -> Ans(Nop)
  | Closure.Int(i) -> Ans(Set(i))
  | Closure.Sub(x, y) -> Ans(Sub(x, V(y)))
  | Closure.Let((x, t1), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x t1 env) e2 in
      concat e1' (x, t1) e2'
  | Closure.Var(x) ->
      (match M.find x env with
        | Type.Unit -> Ans(Nop)
        | _ -> Ans(Mov(x)))
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) ->
      let e2' = g (M.add x t env) e2 in
      let offset, store_fv =
        expand
          (List.map (fun y -> (y, M.find y env)) ys)
          (4, e2')
          (fun y _ offset store_fv -> seq(St(y, x, C(offset)), store_fv))
      in
      let innermost_let = 
          Let((Id.genid "l", Type.Int), 
              SetL(l), 
              seq(St(z, x, C(0)), store_fv)) 
      in
      let mid_let = Let((reg_hp, Type.Int), 
                        Add(reg_hp, C(align offset)), 
                        innermost_let)
      in
      Let((x, t), Mov(reg_hp), mid_let)
  | Closure.AppCls(x, ys) ->
      let int = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallCls(x, int))
  | Closure.AppDir(x, ys) ->
      let int = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallDir(Id.L(x), int))

let h { Closure.name = (Id.L(x), t); 
        Closure.args = yts; 
        Closure.formal_fv = zts; 
        Closure.body = e } =
  let int = separate yts in
  let (offset, load) =
    expand
      zts
      (4, g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
      (fun z t offset load -> Let((z, t), Ld(reg_cl, C(offset)), load))
  in
  match t with
    | Type.Fun(_, t2) ->
        { name = Id.L(x); args = int;  body = load; ret = t2 }
    | _ -> assert false

let f (Closure.Prog(fundefs, e)) =
  data := [];
  let fundefs = List.map h fundefs in
  let e = g M.empty e in
  Prog(!data, fundefs, e)
