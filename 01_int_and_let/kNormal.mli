type t =
  | Int of int
  | Sub of Id.t * Id.t
  | Let of (Id.t * Type.t) * t  * t
  | Var of Id.t

val fv : t -> S.t
val f : Syntax.t -> t
