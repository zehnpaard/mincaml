open KNormal

let find x env = try M.find x env with Not_found -> x

let rec g env = function
  | Let((x, t), e1, e2) ->
      let x' = Id.genid x in
      Let((x', t), g env e1, g (M.add x x' env) e2)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      let env = M.add x (Id.genid x) env in
      let ys = List.map fst yts in
      let env' = M.add_list2 ys (List.map Id.genid ys) env in
      LetRec({ name = (find x env, t);
               args = List.map (fun (y, t) -> (find y env', t)) yts;
               body = g env' e1 },
             g env e2)
  | Unit -> Unit
  | Int(i) -> Int(i)
  | Sub(x, y) -> Sub(find x env, find y env)
  | Var(x) -> Var(find x env)
  | App(x, ys) -> App(find x env, List.map (fun y -> find y env) ys)

let f = g M.empty
