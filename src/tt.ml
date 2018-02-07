module Tm = 
struct
  type idx = Idx of int
  type 'a bind = B of 'a

  type chk = 
  | Up of inf
  | Bool
  | Pi of chk * chk bind
  | Sg of chk * chk bind
  | Eq of chk bind * chk * chk
  | Lam of chk bind
  | Pair of chk * chk
  | Tt
  | Ff
  | Dim0
  | Dim1
  | U
  and inf = 
  | V of idx
  | App of inf * chk
  | Proj1 of inf
  | Proj2 of inf
  | If of chk bind * inf * chk * chk
  | Down of chk * chk
end

module Val = 
struct
  type lvl = Lvl of int

  type nf = 
  | Lam of clo
  | Pair of nf * nf
  | Pi of nf * clo
  | Sg of nf * clo
  | Eq of clo * nf * nf
  | Bool
  | Tt
  | Ff
  | U
  | EDim
  | Dim0
  | Dim1
  | Up of neu ann
  and neu = 
  | Atom of lvl
  | Proj1 of neu
  | Proj2 of neu
  | App of neu * nf ann
  | If of clo * neu * nf * nf
  and env = nf list
  and 'a ann = Ann of 'a * nf
  and clo = Clo of Tm.chk Tm.bind * env
end

module Nbe :
sig
  val eval : Val.env -> Tm.chk -> Val.nf
  val read_nf : int -> Val.nf -> Val.nf -> Tm.chk
  (* val read_ne : Val.neu -> Val.nf -> Tm.chk *)
end = 
struct  

  let lvl_to_idx n (Val.Lvl i) = 
    Tm.Idx (n - (i + 1))

  let rec eval rho t =
    match t with
    | Tm.Up r -> eval_neu rho r
    | Tm.Pi (t, bnd) -> Val.Pi (eval rho t, Val.Clo (bnd, rho))
    | Tm.Sg (t, bnd) -> Val.Sg (eval rho t, Val.Clo (bnd, rho))
    | Tm.Eq (bnd, t1, t2) -> Val.Eq (Val.Clo (bnd, rho), eval rho t1, eval rho t2)
    | Tm.Bool -> Val.Bool
    | Tm.Lam bnd -> Val.Lam (Val.Clo (bnd, rho))
    | Tm.Tt -> Val.Tt
    | Tm.Ff -> Val.Ff
    | Tm.U -> Val.U
    | Tm.Pair (t1, t2) -> Val.Pair (eval rho t1, eval rho t2)
    | Tm.Dim0 -> Val.Dim0
    | Tm.Dim1 -> Val.Dim1
    
  and eval_neu rho r =
    match r with 
    | Tm.V (Tm.Idx i) -> List.nth rho i
    | Tm.App (r, t) -> apply (eval_neu rho r) (eval rho t)
    | Tm.Proj1 r -> proj1 (eval_neu rho r)
    | Tm.Proj2 r -> proj2 (eval_neu rho r)
    | Tm.Down (t, ty) -> eval rho t
    | Tm.If (bnd, tb, tt, tf) ->
      if_ (bnd, rho) (eval_neu rho tb) (eval rho tt) (eval rho tf)

  and read_nf n ty k = 
    match ty with
    | Val.Pi (a, Val.Clo (Tm.B t, rho)) ->      
      let atom = Val.Up (Val.Ann (Val.Atom (Val.Lvl n), a)) in
      let cod = eval (atom :: rho) t in
      let app = apply k atom in 
      let body = read_nf (n + 1) cod app in
      Tm.Lam (Tm.B body)
    | Val.Sg (a, Val.Clo (Tm.B t, rho)) -> 
      let vpi1 = proj1 k in
      let pi1 = read_nf n a vpi1 in
      let cod = eval (vpi1 :: rho) t in
      let pi2 = read_nf n cod (proj2 k) in
      Tm.Pair (pi1, pi2)
    | Val.Eq (Val.Clo (Tm.B bnd, rho), v0, v1) ->
      let atom = Val.Up (Val.Ann (Val.Atom (Val.Lvl n), Val.EDim)) in
      let cod = eval (atom :: rho) bnd in
      let app = apply k atom in
      let body = read_nf (n + 1) cod app in
      Tm.Lam (Tm.B body)
    | Val.Bool -> failwith ""
    | Val.U -> read_ty n k
    | _ ->
      begin match k with
      | Val.Tt -> Tm.Tt
      | Val.Ff -> Tm.Ff
      | Val.Dim0 -> Tm.Dim0 
      | Val.Dim1 -> Tm.Dim1
      | Val.Up (Val.Ann (r, _)) -> Tm.Up (read_neu n r)
      | _ -> failwith "read_nf"
      end
    
  and read_ty n k =
    match k with 
    | Val.Pi (vdom, Val.Clo (Tm.B bnd, rho)) ->
      let dom = read_ty n vdom in
      let atom = Val.Up (Val.Ann (Val.Atom (Val.Lvl n), vdom)) in
      let vcod = eval (atom :: rho) bnd in
      let cod = read_ty (n + 1) vcod in
      Tm.Pi (dom, Tm.B cod)
    | Val.Sg (vdom, Val.Clo (Tm.B bnd, rho)) ->
      let dom = read_ty n vdom in
      let atom = Val.Up (Val.Ann (Val.Atom (Val.Lvl n), vdom)) in
      let vcod = eval (atom :: rho) bnd in
      let cod = read_ty (n + 1) vcod in
      Tm.Sg (dom, Tm.B cod)
    | Val.Eq (Val.Clo (Tm.B bnd, rho), v0, v1) ->
      let atom = Val.Up (Val.Ann (Val.Atom (Val.Lvl n), Val.EDim)) in
      let vdom = eval (atom :: rho) bnd in
      let dom = read_ty (n + 1) vdom in
      let dom0 = eval (Val.Dim0 :: rho) bnd in
      let dom1 = eval (Val.Dim1 :: rho) bnd in
      let t0 = read_nf n dom0 v0 in
      let t1 = read_nf n dom1 v1 in
      Tm.Eq (Tm.B dom, t0, t1)
    | Val.Bool -> Tm.Bool
    | Val.U -> Tm.U
    | _ -> failwith ""
    

  and read_neu n r =
    match r with
    | Val.Atom lvl -> Tm.V (lvl_to_idx n lvl)
    | Val.Proj1 r -> Tm.Proj1 (read_neu n r)
    | Val.Proj2 r -> Tm.Proj2 (read_neu n r)
    | Val.App (r, Val.Ann (v, ty)) -> Tm.App (read_neu n r, read_nf n ty v)
    | Val.If (Val.Clo (Tm.B bnd, rho), r, vt, vf) ->
      let atom = Val.Up (Val.Ann (Val.Atom (Val.Lvl n), Val.Bool)) in
      let vmot = eval (atom :: rho) bnd in
      let vmot_t = eval (Val.Tt :: rho) bnd in
      let vmot_f = eval (Val.Ff :: rho) bnd in
      let mot = read_ty (n + 1) vmot in
      let tr = read_neu n r in
      let tt = read_nf n vmot_t vt in
      let ff = read_nf n vmot_f vf in
      Tm.If (Tm.B mot, tr, tt, ff)

  and apply v1 v2 =
    match v1 with 
    | Val.Lam (Val.Clo (Tm.B t, rho)) -> eval (v2 :: rho) t
    | Val.Up (Val.Ann (r, Val.Pi (dom, Val.Clo (Tm.B bnd, rho)))) ->
      let cod = eval (v2 :: rho) bnd in
      Val.Up (Val.Ann (Val.App (r, Val.Ann (v2, dom)), cod))
    | _ -> failwith "apply"

  and proj1 v = 
    match v with 
    | Val.Pair (v1, _) -> v1
    | Val.Up (Val.Ann (r, Val.Sg (dom, _))) ->
      Val.Up (Val.Ann (Val.Proj1 r, dom))
    | _ -> failwith "proj1"

  and proj2 v = 
    match v with 
    | Val.Pair (_, v2) -> v2
    | Val.Up (Val.Ann (r, Val.Sg (dom, Val.Clo (Tm.B bnd, rho)))) ->
      let cod = eval (proj1 v :: rho) bnd in
      Val.Up (Val.Ann (Val.Proj2 r, cod))
    | _ -> failwith "proj2"

  and if_ (bnd, rho) vb vt vf =
    match vb with 
    | Val.Tt -> vt
    | Val.Ff -> vf
    | Val.Up (Val.Ann (r, _)) ->
      Val.Up (Val.Ann (Val.If (Val.Clo (bnd, rho), r, vt, vf), failwith ""))
    | _ -> failwith "if_" 
end