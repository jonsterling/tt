
module Ex1 =
struct
  open Tm

  let tm =
    Lam (Bind.Mk (Up Var))

  let ty =
    Pi (Unit, Bind.Mk Unit)

  let nf =
    NBE.nbe CNil ~tm:tm ~ty:ty

  let expected =
    Lam (Bind.Mk Ax)

  let test =
    if Tm.equal_chk nf expected then () else failwith "test failed"
end
