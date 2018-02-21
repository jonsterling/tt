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

module type Elab =
sig
  include Monad.Basic
  val alt : 'a t list -> 'a t

  type hole
  type term
  type subst

  val alloc : term jdg -> hole t
  val ask : ctx:term ctx -> ty:term -> (hole * term) t
  val find : hole -> term jdg t
  val fill : hole -> term -> unit t

  val out : term -> (int, term, subst) term_f t
end
