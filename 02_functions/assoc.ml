open KNormal

let rec f = function
  | Let(xt, e1, e2) ->
      let rec insert = function
        | Let(yt, e3, e4) -> Let(yt, e3, insert e4)
        | LetRec(fundefs, e) -> LetRec(fundefs, insert e)
        | e -> Let(xt, e, f e2)
      in
      insert (f e1)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = xt; args = yts; body = f e1 }, f e2)
  | e -> e
