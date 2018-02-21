type ('var, 'term, 'subst) term_f =
  | Var of 'var
  | Lam of 'term
  | App of 'term * 'term
  | Ax
  | Pi of 'term * 'term
  | Unit
  | Univ

type ('term, 'subst) subst_f =
  | Id
  | Wk
  | Cmp of 'subst * 'subst
  | Ext of 'subst * 'term

type 'a subject =
  | Ret of 'a
  | Ask

type 'a ctx =
  | CNil
  | CExt of 'a ctx * 'a

type 'a jdg =
  | Chk of 'a ctx * 'a subject * 'a

module type Tm =
sig
  type hole
  type term
  type subst

  (* memoized *)
  val subst : sb:subst -> tm:term -> term

  (* this should induce sharing *)
  val into : (int, term, subst) term_f -> term
  val intoS : (term, subst) subst_f -> subst

  val meta : hole -> subst -> term
end

module type ElabCore =
sig
  include Monad.Basic
  val alt : 'a t list -> 'a t

  module Tm : Tm

  val alloc : Tm.term jdg -> Tm.hole t
  val find : Tm.hole -> Tm.term jdg t
  val fill : Tm.hole -> Tm.term -> unit t

  val out : Tm.term -> (int, Tm.term, Tm.subst) term_f t
end
