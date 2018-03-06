open Signature

module Id = struct
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
end = struct
  module F = Sig

  module T = struct
    type t =
      | Var of int
      | In of t Sig.t
    [@@deriving (compare, hash, sexp, show)]
  end

  module M = Id

  let var i = T.Var i

  let into tf = T.In tf

  let out t =
    match t with
    | T.Var i -> `V i
    | T.In tf -> `F tf

  let rec weaken n sb =
    match n with
    | 0 -> sb
    | _ -> weaken (n - 1) @@ Subst.ext (Subst.cmp sb Subst.wk) (T.Var 0)

  let rec subst (t, sb) =
    match Subst.out sb, t with
    | Subst.F.Id, _ -> t
    | _, T.Var i -> proj sb i
    | _, T.In tf -> T.In (Sig.map (fun i a -> subst (a, weaken i sb)) tf)

  and proj sb ix =
    match Subst.out sb with
    | Subst.F.Id -> T.Var ix
    | Subst.F.Cmp (sb1, sb0) -> subst (proj sb0 ix, sb1)
    | Subst.F.Ext (sb, t) -> if ix = 0 then t else proj sb (ix - 1)
    | Subst.F.Wk -> T.Var (ix + 1)

  let rec pretty fmt t =
    match t with
    | T.Var i -> Fmt.pf fmt "#%i" i
    | T.In tf -> Sig.pretty pretty fmt tf
end
