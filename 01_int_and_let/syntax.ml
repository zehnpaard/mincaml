type t =
  | Int of int
  | Sub of t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
