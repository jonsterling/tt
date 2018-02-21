open TT
open Elab
open Sigs

module E = Elab (ElabCore)

module Let_syntax =
struct
  let bind = E.bind
end

let pi alpha =
  match%bind E.find alpha with
  | Chk (cx, Ask, ty) ->
    begin match%bind E.out ty with
    | Univ ->
      let%bind (hdom, dom) = E.ask cx ty in
      let%bind (hcod, cod) = E.ask (CExt (cx, dom)) ty in
      let%bind _ = E.fill alpha @@ E.Tm.into @@ Pi (dom, cod) in
      E.return (hdom, hcod)
    | _ -> failwith ""
    end
  | _ -> failwith ""


let lambda alpha =
  match%bind E.find alpha with
  | Chk (gm, Ask, ty) ->
    begin match%bind E.out ty with
    | Pi (dom, cod) ->
      let%bind (hbdy, bdy) = E.ask (CExt (gm, dom)) cod in
      let%bind _ = E.fill alpha @@ E.Tm.into @@ Lam bdy in
      E.return hbdy
    | _ -> failwith ""
    end
  | _ -> failwith ""
