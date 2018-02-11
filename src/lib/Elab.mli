(* Addresses of subgoals, for navigating the proof tree *)
module Addr :
sig
  type t =
    | Pair1
    | Pair2
end

type state
type tactic = state -> state

(* functions to navigate the proof tree *)
val up : tactic
val down : Addr.t -> tactic

(* from OLEG / Idris *)
val attack : tactic
