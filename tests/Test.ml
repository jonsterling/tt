open TT
open Elab
open Sigs

module E = Elab (ElabCore)

module Let_syntax =
struct
  let bind = E.bind
end

let pi key =
  match%bind E.match_goal key with
  | cx, Univ ->
    let ty = E.Tm.into Univ in
    let%bind (hdom, dom) = E.ask cx ty in
    let%bind (hcod, cod) = E.ask (CExt (cx, dom)) ty in
    let%bind _ = E.fill key @@ E.Tm.into @@ Pi (dom, cod) in
    E.return (hdom, hcod)
  | _ -> failwith ""

let lambda key =
  match%bind E.match_goal key with
  | cx, Pi (dom, cod) ->
    let%bind (hbdy, bdy) = E.ask (CExt (cx, dom)) cod in
    let%bind _ = E.fill key @@ E.Tm.into @@ Lam bdy in
    E.return hbdy
  | _ -> failwith "lambda"


let example =
  let%bind kty = E.alloc @@ Chk (CNil, Ask, E.Tm.into Univ) in
  let%bind (kdom, kcod) = pi kty in
  let%bind _ = E.fill kdom @@ E.Tm.into Unit in
  let%bind _ = E.fill kcod @@ E.Tm.into Unit in
  E.return kty

let foo =
  let%bind key = example in
  match%bind E.find key with
  | Chk (_, Ret tm, _) -> E.pretty Fmt.stdout tm
  | _ -> failwith ""

let test = E.run foo




open Cleanup


module LC =
struct
  type 'a t =
    | Lam of 'a
    | App of 'a * 'a

  let map ~f t =
    match t with
    | Lam a -> Lam (f 1 a)
    | App (a1, a2) -> App (f 0 a1, f 0 a2)

  let pp ~ih fmt t =
    match t with
    | Lam a -> Fmt.pf fmt "(lam %a)" ih a
    | App (a0, a1) -> Fmt.pf fmt "(app %a %a)" ih a0 ih a1
end

module LCPure = Pure (LC)

let lam t = LCPure.into @@ LC.Lam t
let app t1 t2 = LCPure.into @@ LC.App (t1, t2)
let var = LCPure.var

let test_tm = lam @@ app (var 0) (var 0)
let _ = LCPure.pp Fmt.stdout test_tm
