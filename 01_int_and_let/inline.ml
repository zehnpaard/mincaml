open KNormal

let threshold = ref 0

let rec size = function
  | Let(_, e1, e2)
  | _ -> 1

let rec g env = function
  | Let(xt, e1, e2) -> Let(xt, g env e1, g env e2)
  | e -> e

let f e = g M.empty e
