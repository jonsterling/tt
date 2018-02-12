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
    | LamBody
    | PiDom
    | PiCod
end

type frame = {ctx : Tm.ctx; ty : Tm.chk; tm : Tm.chk}
type state = Cut of frame * (Tm.chk -> state) option
type tactic = state -> state

let (<:) f k = Cut (f, Some k)
let (<:?) f k = Cut (f, k)

let up (st : state) : state =
  match st with
  | Cut ({tm; _}, Some k) -> k tm
  | _ -> failwith "up"

let rec unload (st : state) : Tm.chk =
  match st with
  | Cut ({tm; _}, Some k) -> unload @@ k tm
  | Cut ({tm; _}, None) -> tm

let down (addr : Addr.t) (st : state) : state =
  match addr, st with
  | Addr.Pair1, Cut ({ctx; tm = Tm.Pair (t1, t2); ty = (Tm.Sg (dom, Tm.Bind.Mk _cod)) as ty}, kont) ->
    {ctx = ctx; tm = t1; ty = dom} <: fun x1 ->
      {ctx = ctx; tm = Tm.Pair (x1, t2); ty = ty} <:? kont
  | Addr.Pair2, Cut ({ctx; tm = Tm.Pair (t1, t2); ty = (Tm.Sg (_dom, Tm.Bind.Mk cod)) as ty}, kont) ->
    let cod' = NBE.nbe ctx ~ty:Tm.U ~tm:(Tm.ChkSub (cod, Tm.Ext (Tm.Id, t1))) in
    {ctx = ctx; tm = t2; ty = cod'} <: fun x2 ->
      {ctx = ctx; tm = Tm.Pair (t1, x2); ty = ty} <:? kont

  | Addr.LamBody, Cut ({ctx; tm = Tm.Lam (Tm.Bind.Mk bdy); ty = (Tm.Pi (dom, Tm.Bind.Mk cod)) as ty}, kont) ->
    {ctx = Tm.CExt (ctx, dom); tm = bdy; ty = cod}  <: fun b ->
      {ctx = ctx; tm = Tm.Lam (Tm.Bind.Mk b); ty = ty} <:? kont

  | Addr.PiDom, Cut ({ctx; tm = Tm.Pi (dom, cod); ty = Tm.U}, kont) ->
    {ctx = ctx; tm = dom; ty = Tm.U} <: fun a ->
      {ctx = ctx; tm = Tm.Pi (a, cod); ty = Tm.U} <:? kont

  | Addr.PiCod, Cut ({ctx; tm = Tm.Pi (dom, Tm.Bind.Mk cod); ty = Tm.U}, kont) ->
    {ctx = Tm.CExt (ctx, dom); tm = cod; ty = Tm.U} <: fun b ->
      {ctx = ctx; tm = Tm.Pi (dom, Tm.Bind.Mk b); ty = Tm.U} <:? kont

  | _ -> failwith "down"

let lift (f : frame -> frame) : tactic =
  function Cut (frm, kont) ->
    f frm <:? kont

let attack : tactic =
  function Cut ({ctx; tm; ty}, kont) ->
    match tm with
    | Tm.Hole (hty, hbdy) ->
      {ctx = ctx; tm = id_hole hty; ty = hty} <: fun h ->
        {ctx = ctx; tm = guess ~ty:hty ~tm:h ~bdy:hbdy; ty = ty} <:? kont
    | _ -> failwith "attack"

let normalize : tactic =
  lift @@ fun {ctx; tm; ty} ->
    {ctx = ctx; tm = NBE.nbe ctx ~tm ~ty; ty = NBE.nbe ctx ~tm:ty ~ty:Tm.U}

let try_ t : tactic =
  lift @@ fun {ctx; tm; ty} ->
    match tm with
    | Tm.Hole (_hty, bdy) ->
      (* TODO: check t against hty, once we have the core typechecker *)
      {ctx = ctx; tm = guess ~ty:ty ~tm:t ~bdy:bdy; ty = ty}
    | _ -> failwith "try_"

let solve : tactic =
  lift @@ fun {ctx; tm; ty} ->
    match tm with
    | Tm.Guess (_, htm, Tm.Bind.Mk hbdy) ->
      {ctx = ctx; tm = Tm.ChkSub (hbdy, Tm.Ext (Tm.Id, htm)); ty = ty}
    | _ -> failwith "solve"

let lambda : tactic =
  lift @@ fun {ctx; tm; _} ->
    match tm with
    | Tm.Hole (ty, Tm.Bind.Mk (Tm.Up Tm.Var)) ->
      begin match NBE.nbe ctx ~tm:ty ~ty:Tm.U with
      | (Tm.Pi (_dom, Tm.Bind.Mk cod)) as nty ->
        let hbdy = id_hole ~ty:cod in
        {ctx = ctx; ty = nty; tm = Tm.Lam (Tm.Bind.Mk hbdy)}
      | nty -> failwith @@ "lambda: " ^ Tm.show_chk nty
      end
    | _ -> failwith @@ "lambda: " ^ Tm.show_chk tm

let pi : tactic =
  lift @@ fun {ctx; tm; _} ->
    match tm with
    | Tm.Hole (ty, Tm.Bind.Mk (Tm.Up Tm.Var)) ->
      begin match NBE.nbe ctx ~tm:ty ~ty:Tm.U with
      | Tm.U ->
        let hdom = id_hole ~ty:Tm.U in
        let hcod = id_hole ~ty:Tm.U in
        {ctx = ctx; ty = Tm.U; tm = Tm.Pi (hdom, Tm.Bind.Mk hcod)}
      | _ -> failwith "pi"
      end
    | _ -> failwith "pi"

let (|>) (tac1 : tactic) (tac2 : tactic) st =
  tac2 (tac1 st)

let init ty =
  {ctx = Tm.CNil; tm = id_hole ty; ty = ty}
    <:? None

let test_script =
  attack
  |> lambda
  |> down Addr.LamBody
  |> try_ (Tm.Up Tm.Var)
  |> solve
  |> up
  |> up
  |> solve
  |> normalize

let test_result =
  unload @@ test_script @@ init @@ Tm.Pi (Tm.Unit, Tm.Bind.Mk Tm.Unit)
