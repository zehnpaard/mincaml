type t =
  | Unit
  | Int of int
  | Sub of Id.t * Id.t
  | Let of (Id.t * Type.t) * t  * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

val fv : t -> S.t
val f : Syntax.t -> t
