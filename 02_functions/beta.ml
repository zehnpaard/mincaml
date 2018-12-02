open KNormal

let find x env = try M.find x env with Not_found -> x

let rec g env = function
  | Let((x, t), e1, e2) ->
      (match g env e1 with
         | Var(y) -> g (M.add x y env) e2
         | e1' -> Let((x, t), e1', g env e2))
  | Var(x) -> Var(find x env)
  | Unit -> Unit
  | Int(i) -> Int(i)
  | Sub(x, y) -> Sub(find x env, find y env)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = xt; args = yts; body = g env e1 }, g env e2)
  | App(x, ys) -> App(find x env, List.map (fun y -> find y env) ys)

let f = g M.empty
