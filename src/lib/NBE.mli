open Tm
open Val

val eval : env -> chk -> nf
val read_nf : int -> nf -> nf -> chk