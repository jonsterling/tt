open Signature
open Model
open EnvMonad

(* This is a model with references to holes *)
module ProofState (Mon : EnvMonad) (Sig : Signature) : sig
  module T : sig
    type t
    [@@deriving (compare, sexp, show)]
  end

  module Subject : sig
    (* A subject is Ask if it has not been refined yet; it is Ret if it has been refined.
       The information order is that [Ask <= Ret t]. *)
    type t =
      | Ask
      | Ret of T.t
    [@@deriving (compare, sexp, show)]
  end

  module Jdg : sig
    type t = {
      cx : T.t list;
      ty : T.t;
      hole : Subject.t;
    }
    [@@deriving (compare, sexp, show)]
  end

  module TermF : sig
    type 'a t = 'a Sig.t
    [@@deriving (compare, sexp, show)]
  end

  module M : Monad.S
    with type 'a t = ('a, Jdg.t) Mon.T.t

  include EffectfulTermModel
    with module F := TermF
     and module M := M
     and module T := T

  val hole : Mon.Key.t -> T.t

  val out : T.t -> [`F of T.t TermF.t | `V of int] M.t
end
