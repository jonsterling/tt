(* We can start by implementing the "attack" tactic from OLEG and Idris;
   but to do this for real, we need to work with the zipper of Tm.chk
   rather than Tm.chk itself, since otherwise we will not know where
   the focus is.

   This would be a fancier and cleaner alternative to the global "hole queue"
   that is used in Idris.
*)

val attack : Tm.chk -> Tm.chk
