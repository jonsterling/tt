open Signature

module Id = Monad.Ident

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

  module M : Monad.S
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

module ExplicitSubst (Sig : Signature) : sig
  include TermModel
    with module F = Sig
end = struct
  module F = Sig
  module M = Id

  module T = struct
    type node =
      | Var of int
      | In of closure ref Sig.t

    and closure =
      | Ret of node
      | Clo of (node, closure ref) Subst.Tensor.t
    [@@deriving (compare, sexp, show)]

    type t = closure ref
    [@@deriving (compare, sexp, show)]
  end

  let var i =
    ref (T.Ret (T.Var i))

  let rec weaken n sb =
    match n with
    | 0 -> sb
    | _ -> weaken (n - 1) @@ Subst.ext (Subst.cmp sb Subst.wk) (var 0)

  let into tf = ref (T.Ret (T.In tf))

  let subst (t, sb) =
    match Subst.out sb, !t with
    | Subst.F.Id, _ -> t
    | _, T.Clo (t, sb') -> ref (T.Clo (t, Subst.cmp sb sb'))
    | _, T.Ret t -> ref (T.Clo (t, sb))

  let rec unwrap : T.t -> T.node =
    fun r ->
      match !r with
      | T.Clo tensor ->
        let node = subst_node tensor in
        r := T.Ret node;
        node
      | T.Ret node -> node

  and subst_ix : (int, T.t) Subst.Tensor.t -> T.node =
    fun (ix, sb) ->
      match Subst.out sb with
      | Subst.F.Id ->
        T.Var ix
      | Subst.F.Cmp (sb1, sb0) ->
        subst_node (subst_ix (ix, sb0), sb1)
      | Subst.F.Ext (sb, t) ->
        if ix = 0 then unwrap t else subst_ix (ix - 1, sb)
      | Subst.F.Wk ->
        T.Var (ix + 1)

  and subst_node : (T.node, T.t) Subst.Tensor.t -> T.node =
    fun (node, sb) ->
      match node with
      | T.In tf -> T.In (Sig.map (fun i a -> subst (a, weaken i sb)) tf)
      | T.Var ix -> subst_ix (ix, sb)

  let out t =
    match unwrap t with
    | T.Var ix -> `V ix
    | T.In tf -> `F tf

  let rec pretty fmt t =
    match unwrap t with
    | T.Var ix -> Fmt.pf fmt "#%i" ix
    | T.In tf -> Sig.pretty pretty fmt tf
end
