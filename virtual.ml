open Asm

let data = ref []

let classify xts ini addf addi =
  let f acc (x, t) = match t with
    | Type.Unit -> acc
    | Type.Float -> addf acc x
    | _ -> addi acc x t
  in
  List.fold_left f ini xts

let separate xts =
  classify
    xts
    ([], [])
    (fun (int, float) x -> (int, float @ [x]))
    (fun (int, float) x _ -> (int @ [x], float))

let expand xts ini addf addi =
  classify
    xts
    ini
    (fun (offset, acc) x ->
       let offset = align offset in
       (offset + 8, addf x offset acc))
    (fun (offset, acc) x t ->
       (offset + 4, addi x t offset acc))

let rec g env = function
  | Closure.Unit -> Ans(Nop)
  | Closure.Int(i) -> Ans(Set(i))
  | Closure.Float(d) ->
      let l =
        try
          let l = List.find (fun (_, d') -> d = d') !data in
          l
        with Not_found ->
          let l = Id.L(Id.genid "l") in
          data := (l, d) :: !data;
          l
      in
      let x = Id.genid "l" in
      Let((x, Type.Int), SetL(l), Ans(LdDF(x, C(0))))
  | Closure.Neg(x) - Ans(Neg(x))
  | Closure.Add(x, y) -> Ans(Add(x, V(y)))
  | Closure.Sub(x, y) -> Ans(Sub(x, V(y)))
  | Closure.FNeg(x) - Ans(FNeg(x))
  | Closure.FAdd(x, y) -> Ans(FAdd(x, V(y)))
  | Closure.FSub(x, y) -> Ans(FSub(x, V(y)))
  | Closure.FMul(x, y) -> Ans(FMul(x, V(y)))
  | Closure.FDiv(x, y) -> Ans(FDiv(x, V(y)))
  | Closure..IfEq(x, y, e1, e2) ->
      (match M.find x env with
        | Type.Bool
        | Type.Int -> Ans(IfEq(x, V(y), g env e1, g env e2))
        | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE(x, y, e1, e2) ->
      (match M.find x env with
        | Type.Bool
        | Type.Int -> Ans(IfLE(x, V(y), g env e1, g env e2))
        | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.Let((x, t1), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x t1 env) e2 in
      concat e1' (x, t1) e2'
  | Closure.Var(x) ->
      (match M.find x env with
        | Type.Unit -> Ans(Nop)
        | Type.Float -> Ans(FMovD(x))
        | _ -> Ans(Mov(x)))
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) ->
      let e2' = g (M.add x t env) e2 in
      let offset, store_fv =
        expand
          (List.map (fun y -> (y, M.find y env)) ys)
          (4, e2')
          (fun y offset store_fv -> seq(StDF(y, x, C(offset)), store_fv))
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
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallCls(x, int, float))
  | Closure.AppDir(x, ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallDir(Id.L(x), int, float))
  | Closure.Tuple(xs) ->
      let y = Id.genid "t" in
      let (offset, store) =
        expand
          (List.map (fun x -> (x, M.find x env)) xs)
          (0, Ans(Mov(y)))
          (fun x offset store -> seq(StDF(x, y, C(offset)), store))
          (fun x _ offset store -> seq(St(x, y, C(offset)), store))
      in
      let inner_let = Let((reg_hp, Type.Int), Add(reg_hp, C(align offset)), store) in
      let tuple_types = List.map (fun x -> M.find x env) xs in
      Let((y, Type.Tuple(tuple_types)), Mov(reg_hp), inner_let)
  | Closure.LetTuple(xts, y, e2) ->
      let s = Closure.fv e2 in
      let (offset, load) =
        expand
          xts
          (0, g (M.add_list xts env) e2)
          (fun x offset load ->
             if not (S.mem x s) then load 
             else fletd(x, LdDF(y, C(offset)), load))
          (fun x t offset load ->
             if not (S.mem x s) then load
             else Let((x, t), Ld(y, C(offset)), load))
      in
      load
  | Closure.Get(x, y) ->
      let offset = Id.genid "o" in
      (match M.find x env with
         | Type.Array(Type.Unit) -> Ans(Nop)
         | Type.Array(Type.Float) ->
             Let((offset, Type.Int), SLL(y, C(3)), Ans(LdDF(x, V(offset))))
         | Type.Array(_) ->
             Let((offset, Type.Int), SLL(y, C(2)), Ans(Ld(x, V(offset))))
         | _ -> assert false)
  | Closure.Put(x, y, z) ->
      let offset = Id.genid "o" in
      (match M.find x env with
         | Type.Array(Type.Unit) -> Ans(Nop)
         | Type.Array(Type.Float) ->
             Let((offset, Type.Int), SLL(y, C(3)), Ans(StDF(z, x, V(offset))))
         | Type.Array(_) ->
             Let((offset, Type.Int), SLL(y, C(2)), Ans(St(z, x, V(offset))))
         | _ -> assert false)
  | Closure.ExtArray(Id.L(x)) -> Ans(SetL(Id.L("min_caml_" ^ x)))

let h { Closure.name = (Id.L(x), t); 
        Closure.args = yts; 
        Closure.formal_fv = zts; 
        Closure.body = e } =
  let (int, float) = separate yts in
  let (offset, load) =
    expand
      zts
      (4, g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
      (fun z offset load -> fletd(z, LdDF(reg_cl, C(offset)), load))
      (fun z t offset load -> Let((z, t), Ld(reg_cl, C(offset)), load))
  in
  match t with
    | Type.Fun(_, t2) ->
        { name = Id.L(x); args = int; fargs = float; body = load; ret = t2 }
    | _ -> assert false

let f (Closure.Prog(fundefs, e)) =
  data := [];
  let fundefs = List.map h fundefs in
  let e = g M.empty e in
  Prog(!data, fundefs, e)
