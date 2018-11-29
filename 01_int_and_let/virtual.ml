open Asm

let rec g env = function
  | Closure.Int(i) -> Ans(Set(i))
  | Closure.Sub(x, y) -> Ans(Sub(x, V(y)))
  | Closure.Let((x, t1), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x t1 env) e2 in
      concat e1' (x, t1) e2'
  | Closure.Var(x) -> Ans(Mov(x))

let f (Closure.Prog(e)) =
  Prog(g M.empty e)
