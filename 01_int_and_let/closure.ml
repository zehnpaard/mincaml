type t =
  | Int of int
  | Sub of Id.t * Id.t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
type prog = Prog of t

let rec g env known = function
  | KNormal.Int(i) -> Int(i)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.Let((x, t), e1, e2) -> 
      Let((x, t), g env known e1, g (M.add x t env) known e2) 
  | KNormal.Var(x) -> Var(x)

let f e = Prog(g M.empty S.emtpy e)
