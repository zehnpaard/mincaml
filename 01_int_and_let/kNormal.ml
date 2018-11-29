type t =
  | Int of int
  | Sub of Id.t * Id.t
  | Let of (Id.t * Type.t) * t  * t
  | Var of Id.t

let rec fv = function
  | Int(_) -> S.empty
  | Sub(x, y) -> S.of_list [x; y]
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x

let insert_let (e, t) k = match e with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let e', t' = k x in
      Let((x, t), e, e'), t'

let rec g env = function
  | Syntax.Int(i) -> Int(i), Type.Int
  | Syntax.Sub(e1, e2) ->
      let f x -> insert_let (g env e2) (fun y -> Sub(x, y), Type.Int) in
      insert_let (g env e1) f
  | Syntax.Let((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
      Let((x, t), e1', e2'), t2
  | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env
  | Syntax.Var(x) -> failwith ("unknown variable:" ^ x)

let f e = fst (g M.empty e)
