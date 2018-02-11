(* Addresses of subgoals, for navigating the proof tree *)
module Addr :
sig
  type t =
    | Pair1
    | Pair2
end

type state
type tactic

(* functions to navigate the proof tree *)
val up : tactic
val down : Addr.t -> tactic

(* from OLEG / Idris *)
val attack : tactic
val lambda : tactic
val pi : tactic

(* This stuff kind of works, but it doesn't seem compatible with the birectional term language;
   that is, it seems more natural to store type information in the lambda, since otherwise we don't
   know how to extend the context when descending through the proof state.

   Need to figure out what's going on... *)
