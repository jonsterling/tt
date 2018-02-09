module D = Val

let rec eval rho t =
  match t with
  | Tm.Pi (dom, cod) -> D.Pi (eval rho dom, D.Clo (cod, rho))
  | Tm.Sg (dom, cod) -> D.Sg (eval rho dom, D.Clo (cod, rho))
  | Tm.Eq (dom, t1, t2) -> D.Eq (D.Clo (dom, rho), eval rho t1, eval rho t2)
  | Tm.U -> D.U
  | Tm.Bool -> D.Bool
  | Tm.Lam bnd -> D.Clo (bnd, rho)
  | Tm.Pair (t1, t2) -> D.Pair (eval rho t1, eval rho t2)
  | Tm.Tt -> D.Tt
  | Tm.Ff -> D.Ff
  | Tm.Dim0 -> D.Dim0
  | Tm.Dim1 -> D.Dim1
  | Tm.Up _ -> failwith "todo: up"
  | Tm.Sub (t, s) ->
    let rho' = eval_sub rho s in
    eval rho' t

and eval_sub rho s =
  match s with
  | Tm.Id -> rho
  | Tm.Wk -> List.tl rho
  | Tm.Cmp (s1, s2) ->
    let rho' = eval_sub rho s2 in
    let rho'' = eval_sub rho' s1 in
    rho''
  | Tm.Ext (s, t) ->
    let rho' = eval_sub rho s in
    let d = eval rho t in
    d :: rho'
