(* Addresses of subgoals, for navigating the proof tree *)
module Addr :
sig
  type t =
    | Pair1
    | Pair2
    (* | LamBody
       | PiDom
       | PiCod *)
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
