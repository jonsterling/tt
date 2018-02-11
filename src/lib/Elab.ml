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

type state = Cut of Tm.ctx * Tm.chk * (Tm.chk -> state) option
type tactic = state -> state

let (<:) (ctx, tm) k = Cut (ctx, tm, Some k)
let (<:?) (ctx, tm) k = Cut (ctx, tm, k)

let up (st : state) : state =
  match st with
  | Cut (ctx, tm, Some k) -> k tm
  | _ -> failwith "up"

let down (addr : Addr.t) (st : state) : state =
  match addr, st with
  | Pair1, Cut (ctx, Tm.Pair (t1, t2), kont) ->
    (ctx, t1) <: fun x1 ->
      (ctx, Tm.Pair (x1, t2)) <:? kont

  | Pair2, Cut (ctx, Tm.Pair (t1, t2), kont) ->
    (ctx, t2) <: fun x2 ->
      (ctx, Tm.Pair (t1, x2)) <:? kont

  | _ -> failwith "down"

let lift (f : Tm.ctx -> Tm.chk -> Tm.chk) : tactic =
  function Cut (ctx, t, kont) ->
    (ctx, f ctx t) <:? kont

let attack : tactic =
  lift @@ fun ctx t ->
    match t with
    | Tm.Hole (ty, bdy) -> guess ~ty:ty ~tm:(id_hole ty) ~bdy:bdy
    | _ -> failwith "attack"

let lambda : tactic =
  function Cut (ctx, t, kont) ->
    match t with
    | Tm.Hole (ty, Tm.Bind.Mk (Tm.Up Tm.Var)) ->
      begin match NBE.nbe ctx ~tm:ty ~ty:Tm.U with
      | Tm.Pi (dom, Tm.Bind.Mk cod) ->
        (Tm.CExt (ctx, dom), id_hole ~ty:cod) <: fun z ->
          (ctx, Tm.Lam (Tm.Bind.Mk z)) <:? kont
      | _ -> failwith "lambda"
      end
    | _ -> failwith "lambda"

let pi : tactic =
  function Cut (ctx, t, kont) ->
    match t with
    | Tm.Hole (ty, Tm.Bind.Mk (Tm.Up Tm.Var)) ->
      begin match NBE.nbe ctx ~tm:ty ~ty:Tm.U with
      | Tm.U ->
        (ctx, id_hole ~ty:U) <: fun dom ->
          (Tm.CExt (ctx, dom), id_hole ~ty:U) <: fun cod ->
             (ctx, Tm.Pi (dom, Tm.Bind.Mk cod)) <:? kont
      | _ -> failwith "pi"
      end
    | _ -> failwith "pi"

let (|>) (tac1 : tactic) (tac2 : tactic) st =
  tac1 (tac2 st)

let test =
  attack |> lambda
