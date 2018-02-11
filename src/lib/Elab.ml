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

type frame = {ctx : Tm.ctx; ty : Tm.chk; tm : Tm.chk}
type state = Cut of frame * (Tm.chk -> state) option
type tactic = state -> state

let (<:) f k = Cut (f, Some k)
let (<:?) f k = Cut (f, k)

let up (st : state) : state =
  match st with
  | Cut ({tm}, Some k) -> k tm
  | _ -> failwith "up"

let down (addr : Addr.t) (st : state) : state =
  match addr, st with
  | Pair1, Cut ({ctx; tm = Tm.Pair (t1, t2); ty = (Tm.Sg (dom, Tm.Bind.Mk cod)) as ty}, kont) ->
    {ctx = ctx; tm = t1; ty = dom} <: fun x1 ->
      {ctx = ctx; tm = Tm.Pair (x1, t2); ty = ty} <:? kont
  | Pair2, Cut ({ctx; tm = Tm.Pair (t1, t2); ty = (Tm.Sg (dom, Tm.Bind.Mk cod)) as ty}, kont) ->
    let cod' = NBE.nbe ctx ~ty:U ~tm:(Tm.ChkSub (cod, Tm.Ext (Tm.Id, t1))) in
    {ctx = ctx; tm = t2; ty = cod'} <: fun x2 ->
      {ctx = ctx; tm = Tm.Pair (t1, x2); ty = ty} <:? kont

  | _ -> failwith "down"

let lift (f : frame -> frame) : tactic =
  function Cut (frm, kont) ->
    f frm <:? kont

let attack : tactic =
  lift @@ fun {ctx; tm; ty} ->
    match tm with
    | Tm.Hole (ty, bdy) -> {ctx = ctx; tm = guess ~ty:ty ~tm:(id_hole ty) ~bdy:bdy; ty = ty}
    | _ -> failwith "attack"

let lambda : tactic =
  lift @@ fun {ctx; tm; ty} ->
    match tm with
    | Tm.Hole (ty, Tm.Bind.Mk (Tm.Up Tm.Var)) ->
      begin match NBE.nbe ctx ~tm:ty ~ty:Tm.U with
      | (Tm.Pi (dom, Tm.Bind.Mk cod)) as nty ->
        let hbdy = id_hole ~ty:cod in
        {ctx = ctx; ty = nty; tm = Tm.Lam (Tm.Bind.Mk hbdy)}
      | _ -> failwith "lambda"
      end
    | _ -> failwith "lambda"

let pi : tactic =
  lift @@ fun {ctx; tm} ->
    match tm with
    | Tm.Hole (ty, Tm.Bind.Mk (Tm.Up Tm.Var)) ->
      begin match NBE.nbe ctx ~tm:ty ~ty:Tm.U with
      | Tm.U ->
        let hdom = id_hole ~ty:U in
        let hcod = id_hole ~ty:U in
        {ctx = ctx; ty = Tm.U; tm = Tm.Pi (hdom, Tm.Bind.Mk hcod)}
      | _ -> failwith "pi"
      end
    | _ -> failwith "pi"

let (|>) (tac1 : tactic) (tac2 : tactic) st =
  tac1 (tac2 st)

let test =
  attack |> lambda
