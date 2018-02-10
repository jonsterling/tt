
module Ex1 =
struct
  let tm =
    Tm.Lam (Tm.Bind.Mk (Tm.Up Tm.Var))

  let ty =
    Tm.Pi (Tm.Unit, Tm.Bind.Mk Tm.Unit)

  let nf =
    NBE.nbe Tm.CNil ~tm:tm ~ty:ty

  let expected =
    Tm.Lam (Tm.Bind.Mk Tm.Ax)

  let test =
    if Tm.equal_chk nf expected then () else failwith "test failed"
end
