open TT
open Elab
open Sigs

module Let_syntax = 
struct
  let bind = Elab.bind
end

let pi alpha = 
  match%bind Elab.find alpha with 
  | Chk (cx, Ask, ty) ->
    begin match%bind Elab.out ty with 
    | Univ ->
      let%bind (hdom, dom) = Elab.ask cx ty in
      let%bind (hcod, cod) = Elab.ask (CExt (cx, dom)) ty in
      let%bind _ = Elab.fill alpha @@ Tm.into @@ Pi (dom, cod) in
      Elab.return (hdom, hcod)
    | _ -> failwith ""
    end
  | _ -> failwith ""


let lambda alpha =
  match%bind Elab.find alpha with 
  | Chk (gm, Ask, ty) ->
    begin match%bind Elab.out ty with
    | Pi (dom, cod) ->
      let%bind (hbdy, bdy) = Elab.ask (CExt (gm, dom)) cod in
      let%bind _ = Elab.fill alpha @@ Tm.into @@ Lam bdy in
      Elab.return hbdy
    | _ -> failwith ""
    end
  | _ -> failwith ""