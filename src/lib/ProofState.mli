open Signature
open Model
open EnvMonad

(* This is a model with references to holes *)
module ProofState (M : EnvMonad) (S : Signature) : sig
  type t
  [@@deriving (compare, sexp, show)]

  (* A subject is Ask if it has not been refined yet; it is Ret if it has been refined.
     The information order is that [Ask <= Ret t]. *)
  type subject =
    | Ask
    | Ret of t
  [@@deriving (compare, sexp, show)]

  type jdg = {
    cx : t list;
    ty : t;
    hole : subject;
  }
  [@@deriving (compare, sexp, show)]

  type 'a term_f = 'a S.t
  [@@deriving (compare, sexp, show)]

  type 'a m = ('a, jdg) M.t
  [@@deriving (compare, sexp, show)]

  include EffectfulTermModel
    with type 'a f := 'a term_f
     and type 'a m := 'a m
     and type t := t

  val hole : M.key -> t

  val out : t -> [`F of t term_f | `V of int] m
end
