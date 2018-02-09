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
  | Tm.Up t -> eval_inf rho t
  | Tm.Sub (t, s) ->
    let rho' = eval_sub rho s in
    eval rho' t

and eval_inf rho t =
  match t with
  | Tm.Var -> List.hd rho
  | Tm.App (t1, t2) ->
    let d1 = eval_inf rho t1 in
    let d2 = eval rho t2 in
    apply d1 d2
  | Tm.Proj1 t ->
    proj1 (eval_inf rho t)
  | Tm.Proj2 t ->
    proj2 (eval_inf rho t)
  | Tm.If (bnd, tb, t1, t2) ->
    let mot = D.Clo (bnd, rho) in
    let db = eval_inf rho tb in
    let d1 = eval rho t1 in
    let d2 = eval rho t2 in
    if_ mot db d1 d2
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

and if_ mot db d1 d2 =
  match db with
  | D.Tt -> d1
  | D.Ff -> d2
  | D.Up (_, dne) ->
    let mot' = apply mot db in
    let dnf1 = D.Down (apply mot D.Tt, d1) in
    let dnf2 = D.Down (apply mot D.Ff, d2) in
    let cond = D.If (mot, dne, dnf1, dnf2) in
    D.Up (mot', cond)
  | _ -> failwith "if: something we can case on"

and quo_nf n dnf =
  let D.Down (dty, d) = dnf in
  match dty, d with
  | D.Pi (dom, cod), _ ->
    let atom = D.Up (dom, D.Atom n) in
    let app = D.Down (apply cod atom, apply d atom) in
    let body = quo_nf (n + 1) app in
    Tm.Lam (Tm.Bind.Mk body)
  | D.Sg (dom, cod), _ ->
    let d1 = proj1 d in
    let d2 = proj2 d in
    let t1 = quo_nf n (D.Down (dom, d1)) in
    let t2 = quo_nf n (D.Down (apply cod d1, d2)) in
    Tm.Pair (t1, t2)
  | D.Eq (cod, d1, d2), _ ->
    let atom = D.Up (D.EDim, D.Atom n) in
    let app = D.Down (apply cod atom, apply d atom) in
    let body = quo_nf (n + 1) app in
    Tm.Lam (Tm.Bind.Mk body)
  | _, D.U -> Tm.U
  | univ, D.Pi (dom, cod) ->
    let tdom = quo_nf n (D.Down (univ, dom)) in
    let atom = D.Up (dom, D.Atom n) in
    let tcod = quo_nf (n + 1) (D.Down (univ, apply cod atom)) in
    Tm.Pi (tdom, Tm.Bind.Mk tcod)
  | univ, D.Sg (dom, cod) ->
    let tdom = quo_nf n (D.Down (univ, dom)) in
    let atom = D.Up (dom, D.Atom n) in
    let tcod = quo_nf (n + 1) (D.Down (univ, apply cod atom)) in
    Tm.Sg (tdom, Tm.Bind.Mk tcod)
  | univ, D.Eq (cod, d1, d2) ->
    let atom = D.Up (D.EDim, D.Atom n) in
    let tcod = quo_nf (n + 1) (D.Down (univ, apply cod atom)) in
    let t1 = quo_nf n (D.Down (apply cod D.Dim0, d1)) in
    let t2 = quo_nf n (D.Down (apply cod D.Dim1, d2)) in
    Tm.Eq (Tm.Bind.Mk tcod, t1, t2)
  | _, D.Tt -> Tm.Tt
  | _, D.Ff -> Tm.Ff
  | _, D.Dim0 -> Tm.Dim0
  | _, D.Dim1 -> Tm.Dim1
  | _, D.Up (_, dne) -> quo_ne n dne
  | _, _ -> failwith "quo_nf"


and quo_ne n dne = failwith "todo"
