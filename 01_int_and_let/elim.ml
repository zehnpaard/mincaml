open KNormal

let rec f = function
  | Let((x, t), e1, e2) ->
      let e1' = f e1 in
      let e2' = f e2 in
      if S.mem x (fv e2')
      then Let((x, t), e1', e2')
      else e2'
  | e -> e
