open KNormal

let rec f = function
  | Let(xt, e1, e2) ->
      let rec insert = function
        | Let(yt, e3, e4) -> Let(yt, e3, insert e4)
        | e -> Let(xt, e, f e2)
      in
      insert (f e1)
  | e -> e
