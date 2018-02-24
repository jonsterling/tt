open TT

open Cleanup


module LC =
struct
  type 'a t =
    | Lam of 'a
    | App of 'a * 'a
    | Pi of 'a * 'a
    | Unit
    | Univ

  let map ~f t =
    match t with
    | Lam a -> Lam (f 1 a)
    | App (a1, a2) -> App (f 0 a1, f 0 a2)
    | Pi (dom, cod) -> Pi (f 0 dom, f 1 cod)
    | Unit -> Unit
    | Univ -> Univ

  let pp ~ih fmt t =
    match t with
    | Lam a -> Fmt.pf fmt "(lam %a)" ih a
    | App (a0, a1) -> Fmt.pf fmt "(app %a %a)" ih a0 ih a1
    | Pi (dom, cod) -> Fmt.pf fmt "(-> %a %a)" ih dom ih cod
    | Unit -> Fmt.pf fmt "unit"
    | Univ -> Fmt.pf fmt "univ"
end

module LCPure = Pure (LC)

module Tac (Env : EnvMonad) =
struct
  module E = ProofState (Env) (LC)

  module Let_syntax =
  struct
    let bind m ~f = Env.bind m f
  end

  let ask cx ty =
    let%bind key = Env.alloc E.{cx = cx; ty = ty; hole = E.Ask} in
    Env.return (key, E.hole key)

  let fill key tm =
    match%bind Env.find key with
    | E.{cx; ty; hole = E.Ask} ->
      Env.improve key E.{cx = cx; ty = ty; hole = E.Ret tm}
    | _ -> failwith "fill"

  let match_hole key =
    match%bind Env.find key with
    | E.{cx; ty; hole = E.Ask} ->
      let%bind pat = E.out ty in
      Env.return (cx, pat)
    | _ -> failwith "match_hole"

  let lambda key =
    match%bind match_hole key with
    | cx, `F (LC.Pi (dom, cod)) ->
      let%bind (kbdy, bdy) = ask (dom :: cx) cod in
      let%bind _ = fill key @@ E.into @@ LC.Lam bdy in
      Env.return kbdy
    | _ -> failwith "lambda"

end
