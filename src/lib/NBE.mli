open Tm
open Val

val eval : Env.t -> Chk.t -> NF.t
val read_nf : int -> NF.t -> NF.t -> Chk.t