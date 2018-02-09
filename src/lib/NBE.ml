module D = Val

let rec eval rho t =
  match t with
  | Tm.Pi (dom, cod) -> D.Pi (eval rho dom, D.Clo (cod, rho))
  | Tm.Sg (dom, cod) -> D.Sg (eval rho dom, D.Clo (cod, rho))
  | Tm.Eq (cod, t1, t2) -> D.Eq (D.Clo (cod, rho), eval rho t1, eval rho t2)
  | Tm.U -> D.U
  | Tm.Bool -> D.Bool
  | Tm.Lam bnd -> D.Clo (bnd, rho)
  | Tm.Pair (t1, t2) -> D.Pair (eval rho t1, eval rho t2)
  | Tm.Tt -> D.Tt
  | Tm.Ff -> D.Ff
  | Tm.Dim0 -> D.Dim0
  | Tm.Dim1 -> D.Dim1
  | Tm.Up t -> eval_neu rho t
  | Tm.Sub (t, s) ->
    let rho' = eval_sub rho s in
    eval rho' t

and eval_neu rho t =
  match t with
  | Tm.Var -> List.hd rho
  | Tm.App (t1, t2) ->
    let d1 = eval_neu rho t1 in
    let d2 = eval rho t2 in
    apply d1 d2
  | Tm.Proj1 t ->
    proj1 (eval_neu rho t)
  | Tm.Proj2 t ->
    proj2 (eval_neu rho t)
  | Tm.If (bnd, tb, t1, t2) ->
    failwith "todo: Tm.if"
  | Tm.Down (_, tm) ->
    eval rho tm

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

and apply d1 d2 =
  match d1 with
  | D.Clo (Tm.Bind.Mk t, rho) ->
    eval (d2 :: rho) t
  | D.Up (ty, dne) ->
    begin match ty with
    | D.Pi (dom, cod) ->
      let cod' = apply cod d2 in
      let app = D.App (dne, D.Down (dom, d2)) in
      D.Up (cod', app)
    | D.Eq (cod, _, _) ->
      let cod' = apply cod d2 in
      let app = D.App (dne, D.Down (D.EDim, d2)) in
      D.Up (cod', app)
    | _ -> failwith "apply/up: unexpected type"
    end
  | _ -> failwith "apply"

and proj1 d =
  match d with
  | D.Pair (d1, d2) -> d1
  | D.Up (ty, dne) ->
    begin match ty with
    | D.Sg (dom, cod) -> D.Up (dom, D.Proj1 dne)
    | _ -> failwith "proj1/up: unexpected type"
    end
  | _ -> failwith "proj1: not projectible"

and proj2 d =
  match d with
  | D.Pair (d1, d2) -> d2
  | D.Up (ty, dne) ->
    begin match ty with
    | D.Sg (dom, cod) ->
      let cod' = apply cod (proj1 d) in
      D.Up (cod', D.Proj2 dne)
    | _ -> failwith "proj2/up: unexpected type"
    end
  | _ -> failwith "proj2: not projectible"
