type hole = Tm.chk Tm.dev ref
type frame = {ctx : Tm.ctx; dev : Tm.chk Tm.dev ref}

type stack = frame list
type state = {ctx : Tm.ctx; ty : Tm.chk; tm : Tm.chk; stk : stack}

let alloc ~ty =
  ref @@ Tm.Hole (ty, Tm.Bind.Mk (Tm.Up Tm.Var))

let init ~ty =
  let hole = alloc ty in
  let frame = {ctx = Tm.CNil; dev = hole} in
  {ty = ty; ctx = Tm.CNil; tm = Tm.Dev hole; stk = [frame]}

let pop stk =
  match stk with
  | {ctx; dev} :: stk -> (ctx, dev, stk)
  | _ -> failwith "pop"

let lambda {tm; stk; ctx; ty} =
  let hctx, dev, stk = pop stk in
  match !dev with
  | Tm.Hole (hty, Tm.Bind.Mk (Tm.Up Tm.Var)) ->
    begin match NBE.nbe hctx ~ty:Tm.U ~tm:hty with
    | Tm.Pi (dom, Tm.Bind.Mk cod) ->
      let dev' = alloc cod in
      let frm' = {ctx = Tm.CExt (hctx, dom); dev = dev'} in
      dev := Tm.Ret (Tm.Lam (Tm.Bind.Mk (Tm.Dev dev')));
      {tm; stk = frm' :: stk; ctx; ty}
    | _ -> failwith ""
    end
  | _ -> failwith "lambda"

let fill t {tm; stk; ctx; ty} =
  let _, dev, _ = pop stk in
  match !dev with
  | Tm.Hole (hty, bdy) ->
    dev := Tm.Guess (hty, t, bdy);
    {tm; stk; ctx; ty}
  | _ -> failwith "fill"

let solve {tm; stk; ctx; ty} =
  let _, dev, stk' = pop stk in
  match !dev with
  | Tm.Guess (_hty, htm, Tm.Bind.Mk bdy) ->
    (* TODO: purify htm *)
    dev := Tm.Ret (Tm.ChkSub (bdy, Tm.Ext (Tm.Id, htm)));
    {tm; stk = stk'; ctx; ty}
  | _ -> failwith "solve"

let attack {ctx; ty; tm; stk} =
  let hctx, dev, _ = pop stk in
  match !dev with
  | Tm.Hole (hty, bdy) ->
    let dev' = alloc hty in
    let guess = Tm.Dev dev' in
    dev := Tm.Guess (hty, guess, bdy);
    let frm = {ctx = hctx; dev = dev'} in
    {ctx; ty; tm; stk = frm :: stk}
  | _ -> failwith "attack"

let rec lookup_frame r stk =
  let ctx, r', stk' = pop stk in
  if r = r' then {ctx = ctx; dev = r} else lookup_frame r stk'

let norm {tm; ctx; ty; stk} =
  {tm = NBE.nbe ctx ~ty ~tm; ty; ctx; stk}

let (|>) t1 t2 st = t2 (t1 st)


let test_ty = Tm.Pi (Tm.Unit, Tm.Bind.Mk Tm.Unit)


let test_tac =
  attack
  |> lambda
  |> (fill (Tm.Up Tm.Var) |> solve)
  |> solve
  |> norm

let test_state = test_tac @@ init test_ty

