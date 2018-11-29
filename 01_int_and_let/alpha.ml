open KNormal

let find x env = try M.find x env with Not_found -> x

let rec g env = function
  | Let((x, t), e1, e2) ->
      let x' = Id.genid x in
      Let((x', t), g env e1, g (M.add x x' env) e2)
  | Int(i) -> Int(i)
  | Sub(x, y) -> Sub(find x env, find y env)
  | Var(x) -> Var(find x env)

let f = g M.empty
