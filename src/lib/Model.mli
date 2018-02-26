open Signature

module type Model = sig
  (* signature endofunctor *)
  type 'a f

  (* algebra for the signature endofunctor *)
  type t

  val into : t f -> t

  val var : int -> t

  val subst : (t, t) Subst.tensor -> t
end

module type EffectfulTermModel = sig
  include Model

  type 'a m

  val out : t -> [`F of t f | `V of int] m

  val pp : Caml.Format.formatter -> t -> unit m
end

module type TermModel = sig
  include EffectfulTermModel
    with type 'a m := 'a
end

module Pure (S : Signature) : sig
  include TermModel
    with type 'a f = 'a S.t
end
