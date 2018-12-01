type t =
  | Unit
  | Int of int
  | Sub of Id.t * Id.t
  | Let of (Id.t * Type.t) * t  * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec fv = function
  | Unit | Int(_) -> S.empty
  | Sub(x, y) -> S.of_list [x; y]
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
  | App(x, ys) -> S.of_list (x::ys)

let insert_let (e, t) k = match e with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let e', t' = k x in
      Let((x, t), e, e'), t'

let rec g env = function
  | Syntax.Unit -> Unit, Type.Unit
  | Syntax.Int(i) -> Int(i), Type.Int
  | Syntax.Sub(e1, e2) ->
      let f x -> insert_let (g env e2) (fun y -> Sub(x, y), Type.Int) in
      insert_let (g env e1) f
  | Syntax.Let((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
      Let((x, t), e1', e2'), t2
  | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env
  | Syntax.Var(x) -> failwith ("unknown variable: " ^ x)
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (M.add_list yts env') e1 in
      LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
  | Syntax.App(Syntax.Var(f) e2s) when not (M.mem f env) -> 
      failwith ("Unknown function: " ^ f)
  | Syntax.App(e1, e2s) ->
      (match g env e1 with
         | _, Type.Fun(_, t) as g_e1 ->
            insert_let g_e1
              (fun f ->
                 let rec bind xs = function
                   | [] -> App(f, xs), t
                   | e2 :: e2s -> insert_let (g env e2) (fun x -> bind (xs @ [x]) e2s)
                 in
                 bind [] e2s)
         | _ -> assert false)

let f e = fst (g M.empty e)
