type closure = { entry : Id.l; actual_fv : Id.t list }
type t =
  | Unit
  | Int of int
  | Sub of Id.t * Id.t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
type fundef = { name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t }
type prog = Prog of fundef list * t

let rec fv = function
  | Unit
  | Int(_) -> S.empty
  | Sub(x, y) -> S.of_list [x; y]
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls ((x, t), { entry = l; actual_fv = ys }, e) ->
      S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) -> S.of_list xs

let toplevel : fundef list ref = ref []

let rec g env known = function
  | KNormal.LetRec({ KNormal.name = (x, t);
                     KNormal.args = yts;
                     KNormal.body = e1 }, e2) ->
      let toplevel_backup = !toplevel in
      let env' = M.add x t env in
      let known' = S.add x known in
      let e1' = g (M.add_list yts env') known' e1 in
      let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
      let known', e1' =
        if S.is_empty zs
        then known', e1'
        else 
          begin
            toplevel := toplevel_backup;
            let e1' = g (M.add_list yts env') known e1 in
            known, e1'
          end
      in
      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in
      let zts = List.map (fun z -> (z, M.find z env')) zs in
      let toplevelf = { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } in
      toplevel := toplevelf :: !toplevel in
      let e2' = g env' known' e2 in
      if S.mem x (fv e2')
      then MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2')
      else e2'
  | KNormal.App(f, xs) when S.mem f known -> AppDir(Id.L(f), xs)
  | KNormal.App(f, xs) -> AppCls(f, xs)
  | KNormal.Unit -> Unit
  | KNormal.Int(i) -> Int(i)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.Let((x, t), e1, e2) -> 
      Let((x, t), g env known e1, g (M.add x t env) known e2) 
  | KNormal.Var(x) -> Var(x)

let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog(List.rev !toplevel, e')
