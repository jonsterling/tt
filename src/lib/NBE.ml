open Tm
open Val

let lvl_to_idx n (Lvl.Mk i) =
  Idx.Mk (n - (i + 1))

let rec eval rho t =
  match t with
  | Chk.Up r -> eval_neu rho r
  | Chk.Pi (t, bnd) -> NF.Pi (eval rho t, Clo.Mk (bnd, rho))
  | Chk.Sg (t, bnd) -> NF.Sg (eval rho t, Clo.Mk (bnd, rho))
  | Chk.Eq (bnd, t1, t2) -> NF.Eq (Clo.Mk (bnd, rho), eval rho t1, eval rho t2)
  | Chk.Bool -> NF.Bool
  | Chk.Lam bnd -> NF.Lam (Clo.Mk (bnd, rho))
  | Chk.Tt -> NF.Tt
  | Chk.Ff -> NF.Ff
  | Chk.U -> NF.U
  | Chk.Pair (t1, t2) -> NF.Pair (eval rho t1, eval rho t2)
  | Chk.Dim0 -> NF.Dim0
  | Chk.Dim1 -> NF.Dim1

and eval_neu rho r =
  match r with
  | Inf.V (Idx.Mk i) -> List.nth rho i
  | Inf.App (r, t) -> apply (eval_neu rho r) (eval rho t)
  | Inf.Proj1 r -> proj1 (eval_neu rho r)
  | Inf.Proj2 r -> proj2 (eval_neu rho r)
  | Inf.Down (t, ty) -> eval rho t
  | Inf.If (bnd, tb, tt, tf) ->
    if_ (bnd, rho) (eval_neu rho tb) (eval rho tt) (eval rho tf)

and read_nf n ty k =
  match ty with
  | NF.Pi (a, Clo.Mk (Bind.Mk t, rho)) ->
    let atom = NF.Up (Ann.Mk (Neu.Atom (Lvl.Mk n), a)) in
    let cod = eval (atom :: rho) t in
    let app = apply k atom in
    let body = read_nf (n + 1) cod app in
    Chk.Lam (Bind.Mk body)
  | NF.Sg (a, Clo.Mk (Bind.Mk t, rho)) ->
    let vpi1 = proj1 k in
    let pi1 = read_nf n a vpi1 in
    let cod = eval (vpi1 :: rho) t in
    let pi2 = read_nf n cod (proj2 k) in
    Chk.Pair (pi1, pi2)
  | NF.Eq (Clo.Mk (Bind.Mk bnd, rho), v0, v1) ->
    let atom = NF.Up (Ann.Mk (Neu.Atom (Lvl.Mk n), NF.EDim)) in
    let cod = eval (atom :: rho) bnd in
    let app = apply k atom in
    let body = read_nf (n + 1) cod app in
    Chk.Lam (Bind.Mk body)
  | NF.U -> read_ty n k
  | _ ->
    begin match k with
      | NF.Tt -> Chk.Tt
      | NF.Ff -> Chk.Ff
      | NF.Dim0 -> Chk.Dim0
      | NF.Dim1 -> Chk.Dim1
      | NF.Up (Ann.Mk (r, _)) -> Chk.Up (read_neu n r)
      | _ -> failwith "read_nf"
    end

and read_ty n k =
  match k with
  | NF.Pi (vdom, Clo.Mk (Bind.Mk bnd, rho)) ->
    let dom = read_ty n vdom in
    let atom = NF.Up (Ann.Mk (Neu.Atom (Lvl.Mk n), vdom)) in
    let vcod = eval (atom :: rho) bnd in
    let cod = read_ty (n + 1) vcod in
    Chk.Pi (dom, Bind.Mk cod)
  | NF.Sg (vdom, Clo.Mk (Bind.Mk bnd, rho)) ->
    let dom = read_ty n vdom in
    let atom = NF.Up (Ann.Mk (Neu.Atom (Lvl.Mk n), vdom)) in
    let vcod = eval (atom :: rho) bnd in
    let cod = read_ty (n + 1) vcod in
    Chk.Sg (dom, Bind.Mk cod)
  | NF.Eq (Clo.Mk (Bind.Mk bnd, rho), v0, v1) ->
    let atom = NF.Up (Ann.Mk (Neu.Atom (Lvl.Mk n), NF.EDim)) in
    let vdom = eval (atom :: rho) bnd in
    let dom = read_ty (n + 1) vdom in
    let dom0 = eval (NF.Dim0 :: rho) bnd in
    let dom1 = eval (NF.Dim1 :: rho) bnd in
    let t0 = read_nf n dom0 v0 in
    let t1 = read_nf n dom1 v1 in
    Chk.Eq (Bind.Mk dom, t0, t1)
  | NF.Bool -> Chk.Bool
  | NF.U -> Chk.U
  | _ -> failwith ""


and read_neu n r =
  match r with
  | Neu.Atom lvl -> Inf.V (lvl_to_idx n lvl)
  | Neu.Proj1 r -> Inf.Proj1 (read_neu n r)
  | Neu.Proj2 r -> Inf.Proj2 (read_neu n r)
  | Neu.App (r, Ann.Mk (v, ty)) -> Inf.App (read_neu n r, read_nf n ty v)
  | Neu.If (Clo.Mk (Bind.Mk bnd, rho), r, vt, vf) ->
    let atom = NF.Up (Ann.Mk (Neu.Atom (Lvl.Mk n), NF.Bool)) in
    let vmot = eval (atom :: rho) bnd in
    let vmot_t = eval (NF.Tt :: rho) bnd in
    let vmot_f = eval (NF.Ff :: rho) bnd in
    let mot = read_ty (n + 1) vmot in
    let tr = read_neu n r in
    let tt = read_nf n vmot_t vt in
    let ff = read_nf n vmot_f vf in
    Inf.If (Bind.Mk mot, tr, tt, ff)

and apply v1 v2 =
  match v1 with
  | NF.Lam (Clo.Mk (Bind.Mk t, rho)) -> eval (v2 :: rho) t
  | NF.Up (Ann.Mk (r, NF.Pi (dom, Clo.Mk (Bind.Mk bnd, rho)))) ->
    let cod = eval (v2 :: rho) bnd in
    NF.Up (Ann.Mk (Neu.App (r, Ann.Mk (v2, dom)), cod))
  | _ -> failwith "apply"

and proj1 v =
  match v with
  | NF.Pair (v1, _) -> v1
  | NF.Up (Ann.Mk (r, NF.Sg (dom, _))) ->
    NF.Up (Ann.Mk (Neu.Proj1 r, dom))
  | _ -> failwith "proj1"

and proj2 v =
  match v with
  | NF.Pair (_, v2) -> v2
  | NF.Up (Ann.Mk (r, NF.Sg (dom, Clo.Mk (Bind.Mk bnd, rho)))) ->
    let cod = eval (proj1 v :: rho) bnd in
    NF.Up (Ann.Mk (Neu.Proj2 r, cod))
  | _ -> failwith "proj2"

and if_ (bnd, rho) vb vt vf =
  match vb with
  | NF.Tt -> vt
  | NF.Ff -> vf
  | NF.Up (Ann.Mk (r, _)) ->
    NF.Up (Ann.Mk (Neu.If (Clo.Mk (bnd, rho), r, vt, vf), failwith ""))
  | _ -> failwith "if_"
