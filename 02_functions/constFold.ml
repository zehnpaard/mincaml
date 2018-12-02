open KNormal

let memi x env =
  try (match M.find x env with Int(_) -> true | _ -> false)
  with Not_found -> false

let findi x env = match M.find x env with Int(i) -> i | _ -> raise Not_found

let rec g env = function
  | Var(x) when memi x env -> Int(findi x env)
  | Sub(x, y) when memi x env && memi y env -> Int(findi x env - findi y env)
  | Let((x, t), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x e1' env) e2 in
      Let((x, t), e1', e2')
  | LetRec({ name = x; args = ys; body = e1 }, e2) ->
      LetRec({ name = x; args = ys; body = g env e1 }, g env e2)
  | e -> e

let f = g M.empty
