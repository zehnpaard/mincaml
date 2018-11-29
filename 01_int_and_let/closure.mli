type closure = { entry : Id.l; actual_fv : Id.t list }
type t =
  | Int of int
  | Sub of Id.t * Id.t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
type prog = Prog of t

val fv : t -> S.t
val f : KNormal.t -> prog
