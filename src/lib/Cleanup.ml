(* In this file are some ideas to clean up the treatment of abstract syntax *)

type 'term subst =
  | Id
  | Wk
  | Cmp of 'term subst * 'term subst
  | Ext of 'term subst * 'term

type ('a, 'b) tensor = Clo of 'a * 'b subst

module type Signature =
sig
  (* This is meant to be the signature endofunctor *)
  type 'a t

  (* The function takes an extra parameter to indicate underneath how many
     binders it is being called. This feels a little ad-hoc, but it seems
     to suffice. *)
  val map : f:(int -> 'a -> 'b) -> 'a t -> 'b t
end

module LC =
struct
  type 'a t =
    | Lam of 'a
    | App of 'a * 'a

  let map ~f t =
    match t with
    | Lam a -> Lam (f 1 a)
    | App (a1, a2) -> App (f 0 a1, f 0 a2)
end

module type Model =
sig
  type 'a f

  type t
  val var : int -> t
  val into : t f -> t
  val subst : (t, t) tensor -> t
end

module type TermModel =
sig
  include Model
  val out : t -> [`F of t f | `V of int]
end

module Pure (S : Signature) : TermModel with type 'a f = 'a S.t =
struct
  type 'a f = 'a S.t
  type t =
    | Var of int
    | In of t S.t

  let var i = Var i

  let into tf = In tf

  let out t =
    match t with
    | Var i -> `V i
    | In tf -> `F tf

  let rec weaken n sb =
    match n with
    | 0 -> sb
    | _ -> weaken (n - 1) @@ Ext (Cmp (sb, Wk), Var 0)

  let rec subst (Clo (t, sb)) =
    match t with
    | Var i -> proj sb i
    | In tf -> In (S.map (fun i a -> subst @@ Clo (a, weaken i sb)) tf)

  and proj sb ix =
    match sb with
    | Id -> Var ix
    | Cmp (sb1, sb0) -> subst @@ Clo (proj sb0 ix, sb1)
    | Ext (_, t) -> if ix = 0 then t else proj sb (ix - 1)
    | Wk -> Var (ix + 1)
end

(* This is a model with references to holes *)
module Disect (S : Signature) : Model with type 'a f = 'a S.t =
struct
  type 'a f = 'a S.t

  type t =
    | Var of int
    | In of t S.t
    | Ref of string * t subst

  let var i = Var i

  let into tf = In tf

  let rec weaken n sb =
    match n with
    | 0 -> sb
    | _ -> weaken (n - 1) @@ Ext (Cmp (sb, Wk), Var 0)

  let rec subst (Clo (t, sb)) =
    match t with
    | Var i -> proj sb i
    | In tf -> In (S.map (fun i a -> subst @@ Clo (a, weaken i sb)) tf)
    | Ref (key, sb') -> Ref (key, Cmp (sb, sb'))

  and proj sb ix =
    match sb with
    | Id -> Var ix
    | Cmp (sb1, sb0) -> subst @@ Clo (proj sb0 ix, sb1)
    | Ext (_, t) -> if ix = 0 then t else proj sb (ix - 1)
    | Wk -> Var (ix + 1)
end
