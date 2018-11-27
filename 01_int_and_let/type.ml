type t =
  | Int
  | Var of t option ref

let gentyp () = Var (ref None)
