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
  let Cut ({ctx; tm; ty}, kont) = st in
  match addr, NBE.nbe ctx ~ty ~tm, NBE.nbe ctx ~ty:Tm.U ~tm:ty with
  | Addr.Pair1, Tm.Pair (t1, t2), (Tm.Sg (dom, Tm.Bind.Mk _cod) as ty) ->
    {ctx; tm = t1; ty = dom} <: fun x1 ->
      {ctx; tm = Tm.Pair (x1, t2); ty} <:? kont

  | Addr.Pair2, Tm.Pair (t1, t2), (Tm.Sg (_dom, Tm.Bind.Mk cod) as ty) ->
    let cod' = NBE.nbe ctx ~ty:Tm.U ~tm:(Tm.ChkSub (cod, Tm.Ext (Tm.Id, t1))) in
    {ctx; tm = t2; ty = cod'} <: fun x2 ->
      {ctx; tm = Tm.Pair (t1, x2); ty} <:? kont

  | Addr.LamBody, Tm.Lam (Tm.Bind.Mk bdy), (Tm.Pi (dom, Tm.Bind.Mk cod) as ty) ->
    {ctx = Tm.CExt (ctx, dom); tm = bdy; ty = cod}  <: fun b ->
      {ctx; tm = Tm.Lam (Tm.Bind.Mk b); ty} <:? kont

  | Addr.PiDom, Tm.Pi (dom, cod), Tm.U ->
    {ctx; tm = dom; ty = Tm.U} <: fun a ->
      {ctx; tm = Tm.Pi (a, cod); ty = Tm.U} <:? kont

  | Addr.PiCod, Tm.Pi (dom, Tm.Bind.Mk cod), Tm.U ->
    {ctx = Tm.CExt (ctx, dom); tm = cod; ty = Tm.U} <: fun b ->
      {ctx; tm = Tm.Pi (dom, Tm.Bind.Mk b); ty = Tm.U} <:? kont

  | _, tm, _ -> failwith @@ "down" ^ Tm.show_chk tm

let lift (f : frame -> frame) : tactic =
  function Cut (frm, kont) ->
    f frm <:? kont

let attack : tactic =
  function Cut ({ctx; tm; ty}, kont) ->
    match tm with
    | Tm.Hole (hty, hbdy) ->
      {ctx; tm = id_hole hty; ty = hty} <: fun h ->
        {ctx; tm = guess ~ty:hty ~tm:h ~bdy:hbdy; ty} <:? kont
    | _ -> failwith "attack"

let normalize : tactic =
  lift @@ fun {ctx; tm; ty} ->
    {ctx; tm = NBE.nbe ctx ~tm ~ty; ty = NBE.nbe ctx ~tm:ty ~ty:Tm.U}

let try_ t : tactic =
  lift @@ fun {ctx; tm; ty} ->
    match tm with
    | Tm.Hole (_hty, bdy) ->
      (* TODO: check t against hty, once we have the core typechecker *)
      {ctx; tm = guess ~ty ~tm:t ~bdy; ty}
    | _ -> failwith "try_"

let solve : tactic =
  lift @@ fun {ctx; tm; ty} ->
    match tm with
    | Tm.Guess (_, htm, Tm.Bind.Mk hbdy) ->
      {ctx; tm = Tm.ChkSub (hbdy, Tm.Ext (Tm.Id, htm)); ty}
    | _ -> failwith "solve"

let lambda : tactic =
  lift @@ fun {ctx; tm; _} ->
    match tm with
    | Tm.Hole (ty, Tm.Bind.Mk (Tm.Up Tm.Var)) ->
      begin match NBE.nbe ctx ~tm:ty ~ty:Tm.U with
      | (Tm.Pi (_dom, Tm.Bind.Mk cod)) as nty ->
        let hbdy = id_hole ~ty:cod in
        {ctx; ty = nty; tm = Tm.Lam (Tm.Bind.Mk hbdy)}
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
        {ctx; ty = Tm.U; tm = Tm.Pi (hdom, Tm.Bind.Mk hcod)}
      | _ -> failwith "pi"
      end
    | _ -> failwith "pi"

let (|>) (tac1 : tactic) (tac2 : tactic) st =
  tac2 (tac1 st)

let init ty =
  {ctx = Tm.CNil; tm = id_hole ty; ty}
    <:? None

let attack_with tac =
  attack
    |> tac
    |> up
    |> solve

let down_with addr tac =
  down addr
  |> tac
  |> up

let lam bdy =
  attack_with @@
    lambda |> down_with Addr.LamBody @@
      bdy (Tm.Up Tm.Var)

let test_script =
  lam @@ fun x ->
    try_ x
    |> solve


let test_result =
  unload @@ (test_script |> normalize) @@ init @@ Tm.Pi (Tm.Unit, Tm.Bind.Mk Tm.Unit)
