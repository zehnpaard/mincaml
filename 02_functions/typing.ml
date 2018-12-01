open Syntax

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t

let extenv = ref M.empty

let rec deref_type = function
  | Type.Fun(t1s, t2) -> Type.Fun(List.map deref_typ t1s, deref_typ t2)
  | Type.Var({ contents = None } as r) ->
      Format.eprintf "uninstantiated type variable detected; assuming int@.";
      r := Some(Type.Int);
      Type.Int
  | Type.Var({ contents = Some(t) } as r) ->
      let t' = deref_typ t in
      r := Some(t');
      t'
  | t -> t

let rec deref_id_typ (x, t) -> (x, deref_typ t)

let rec deref_term = function
  | Sub(e1, e2) -> Sub(deref_term e1, deref_term e2)
  | Let(xt, e1, e2) -> Let(deref_id_typ xt, deref_term e1, deref_term e2)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = deref_id_typ xt;
               args = List.map deref_typ_id yts;
               body = deref_term e1 },
             deref_term e2)
  | App(e, e2) -> App(deref_term e, List.map deref_term es)
  | e -> e

let rec occur r1 = function
  | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
  | Type.Var(r2) when r1 == r2 -> true
  | Type.Var({ contents = None }) -> false
  | Type.Var({ contents = Some(t2) }) -> occur r1 t2
  | _ -> false

let rec unify t1 t2 = match t1, t2 with
  | Type.Unit, Type.Unit 
  | Type.Int, Type.Int 
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
      (try List.iter2 unify t1s t2s
       with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)));
       unify t1' t2'
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
  | _, Type.Var({ contents = Some(t2') }) -> unify t1 t2'
  | Type.Var({ contents = None } as r1), _ ->
      if occur r1 t2 then raise (Unify(t1, t2));
      r1 := Some(t2)
  | _, Type.Var({ contents = None } as r2)_ ->
      if occur r2 t1 then raise (Unify(t1, t2));
      r2 := Some(t1)
  | _, _ -> raise (Unify(t1, t2))

let rec g env e =
  try
    match e with
      | Unit -> Type.Unit
      | Int(_) -> Type.Int
      | Sub(e1, e2) ->
          unify Type.Int (g env e1);
          unify Type.Int (g env e2);
          Type.Int
      | Let((x, t) e1, e2) ->
          unify t (g env e1);
          g (M.add x t env) e2
      | Var(x) when M.mem x env -> M.find x env
      | Var(x) when M.mem x !extenv -> M.find x !extenv
      | Var(x) ->
          Format.eprintf "free variable %s assumed as external@." x;
          let t = Type.gentyp () in
          extenv := M.add x t !extenv;
          t
      | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
          let env = M.add x t env in
          unify t (Type.Fun(List.map snd yts, g (M.add_list yts env) e1));
          g env e2
      | App(e, es) ->
          let t = Type.gentyp () in
          unify (g env e) (Type.Fun(List.map (g env) es, t));
          t
  with Unify(t1, t2) -> raise (Error(deref_term e, deref_typ t1, deref_typ e2))

let f e =
    extenv := M.empty;
    (try unify Type.Unit (g M.empty e)
     with Unify _ -> failwith "top level does not have type unit");
     extenv := M.map deref_typ !extenv;
     deref_term e
