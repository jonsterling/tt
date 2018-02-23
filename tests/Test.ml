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
