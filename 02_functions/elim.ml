open KNormal

let rec effect = function
  | App _ -> true
  | Let(_, e1, e2) -> effect e1 || effect e2
  | LetRec(_, e) -> effect e
  | _ -> false

let rec f = function
  | Let((x, t), e1, e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      if effect e1' || S.mem x (fv e2')
      then Let((x, t), e1', e2')
      else e2'
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      let e2' = f e2 in
      if S.mem x (fv e2')
      then LetRec({ name = (x, t); args = yts; body = f e1 }, e2')
      else e2'
  | e -> e
