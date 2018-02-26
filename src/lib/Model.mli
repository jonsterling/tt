open Signature

module Id : sig
  type 'a t = 'a
end

module type Model = sig
  module F : sig
    (* signature endofunctor *)
    type 'a t
  end

  module T : sig
    (* algebra for the signature endofunctor *)
    type t
    [@@deriving (compare, sexp, show)]
  end

  val into : T.t F.t -> T.t

  val var : int -> T.t

  val subst : (T.t, T.t) Subst.Tensor.t -> T.t
end

module type EffectfulTermModel = sig
  include Model

  module M : sig
    type 'a t
  end

  val out : T.t -> [`F of T.t F.t | `V of int] M.t

  val pretty : Caml.Format.formatter -> T.t -> unit M.t
end

module type TermModel = sig
  include EffectfulTermModel
    with module M = Id
end

module Pure (Sig : Signature) : sig
  include TermModel
    with module F = Sig
end
