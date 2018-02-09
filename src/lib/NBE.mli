open Tm
open Val

val eval : env -> chk -> d
val eval_inf : env -> inf -> d
val eval_sub : env -> sub -> env

val quo_nf : int -> dnf -> chk
