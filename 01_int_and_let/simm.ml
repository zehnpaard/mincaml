open Asm

let rec g env = function
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t), Set(i), e) when (-4096 <= i) && (i < 4096) ->
      let e' = g (M.add x i env) e in
      if List.mem x (fv e') then Let((x, t), Set(i), e')
      else e'
  | Let(xt, exp, e) -> Let(xt, g' env exp, g env e)
and g' env = function
  | Sub(x, V(y)) when M.mem y env -> Sub(x, C(M.find y env))
  | e -> e

let f (Prog(e)) =
  Prog(g M.empty e)
