open TT
open Model
open EnvMonad
open ProofState


module LC = struct
  type 'a t =
    | Lam of 'a
    | App of 'a * 'a
    | Pair of 'a * 'a
    | Pi of 'a * 'a
    | Sg of 'a * 'a
    | Unit
    | Univ
  [@@deriving (compare, hash, sexp, show)]

  let map ~f t =
    match t with
    | Lam a -> Lam (f 1 a)
    | App (a1, a2) -> App (f 0 a1, f 0 a2)
    | Pair (a1, a2) -> Pair (f 0 a1, f 0 a2)
    | Pi (dom, cod) -> Pi (f 0 dom, f 1 cod)
    | Sg (dom, cod) -> Sg (f 0 dom, f 1 cod)
    | Unit -> Unit
    | Univ -> Univ

  let pretty ~ih fmt t =
    match t with
    | Lam a -> Fmt.pf fmt "@[(lam@ %a)@]" ih a
    | App (a0, a1) -> Fmt.pf fmt "@[(app@ %a@ %a)@]" ih a0 ih a1
    | Pair (a0, a1) -> Fmt.pf fmt "@[(cons@ %a@ %a)@]" ih a0 ih a1
    | Pi (dom, cod) -> Fmt.pf fmt "@[(->@ %a@ %a)@]" ih dom ih cod
    | Sg (dom, cod) -> Fmt.pf fmt "@[(*@ %a@ %a)@]" ih dom ih cod
    | Unit -> Fmt.pf fmt "unit"
    | Univ -> Fmt.pf fmt "univ"
end

module LCPure = Pure (LC)

module Tac (Env : EnvMonad) = struct
  module E = ProofState (Env) (LC)

  module Let_syntax = struct
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

  let pair key =
    match%bind match_hole key with
    | cx, `F (LC.Sg (dom, cod)) ->
      let%bind (k1, t1) = ask cx dom in
      let%bind (k2, t2) = ask cx @@ E.subst (cod, Subst.ext Subst.id t1) in
      let%bind _ = fill key @@ E.into @@ LC.Pair (t1, t2) in
      Env.return (k1, k2)
    | _ -> failwith "pair"
end
