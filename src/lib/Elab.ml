let hole ~ty ~bdy =
  Tm.Hole (ty, bdy)

let guess ~ty ~tm ~bdy =
  Tm.Guess (ty, tm, bdy)

let id_hole ~ty =
  Tm.Hole (ty, Tm.Bind.Mk (Tm.Up Tm.Var))

let attack tm =
  match tm with
  | Tm.Hole (ty, bdy) ->
    guess ~ty:ty ~tm:(id_hole ty) ~bdy:bdy
  | _ -> failwith "attack"
