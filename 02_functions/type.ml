type t =
  | Unit
  | Int
  | Fun of t list * t
  | Var of t option ref

let gentyp () = Var (ref None)
