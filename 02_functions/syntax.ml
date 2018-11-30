type t =
  | Unit
  | Int of int
  | Sub of t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
