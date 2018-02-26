open Signature
open Model
open EnvMonad

(* This is a model with references to holes *)
module ProofState (M : EnvMonad) (S : Signature) : sig
  type t

  (* A subject is Ask if it has not been refined yet; it is Ret if it has been refined.
     The information order is that [Ask <= Ret t]. *)
  type subject =
    | Ask
    | Ret of t

  type jdg = {
    cx : t list;
    ty : t;
    hole : subject;
  }

  type 'a term_f = 'a S.t

  type 'a m = ('a, jdg) M.t

  include EffectfulTermModel
    with type 'a f := 'a term_f
     and type 'a m := 'a m
     and type t := t

  val hole : M.key -> t

  val out : t -> [`F of t term_f | `V of int] m
end
