let hole ~ty ~bdy =
  Tm.Hole (ty, bdy)

let guess ~ty ~tm ~bdy =
  Tm.Guess (ty, tm, bdy)

let id_hole ~ty =
  Tm.Hole (ty, Tm.Bind.Mk (Tm.Up Tm.Var))


module Addr =
struct
  type t =
    | Pair1
    | Pair2
end

type state = Cut of Tm.chk * (Tm.chk -> state) option
type tactic = state -> state

let (<:) t k = Cut (t, Some k)
let (<:?) t k = Cut (t, k)
let ret t = Cut (t, None)

let up (st : state) : state =
  match st with
  | Cut (t, Some k) -> k t
  | _ -> failwith "up"

let down (addr : Addr.t) (st : state) : state =
  match addr, st with
  | Pair1, Cut (Tm.Pair (t1, t2), kont) ->
    t1 <: fun x1 ->
      Tm.Pair (x1, t2) <:? kont

  | Pair2, Cut (Tm.Pair (t1, t2), kont) ->
    t2 <: fun x2 ->
      Tm.Pair (t1, x2) <:? kont

  | _ -> failwith "down"

let lift (f : Tm.chk -> Tm.chk) : tactic =
  function Cut (t, kont) ->
    f t <:? kont

let attack : tactic =
  lift @@ fun t ->
    match t with
    | Tm.Hole (ty, bdy) -> guess ~ty:ty ~tm:(id_hole ty) ~bdy:bdy
    | _ -> failwith "attack"
