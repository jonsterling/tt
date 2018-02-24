open Signature

module type Model =
sig
  (* signature endofunctor *)
  type 'a f

  (* algebra for the signature endofunctor *)
  type t
  val into : t f -> t

  val var : int -> t
  val subst : (t, t) Subst.tensor -> t
end

module type EffectfulTermModel =
sig
  include Model
  type 'a m
  val out : t -> [`F of t f | `V of int] m
  val pp : Caml.Format.formatter -> t -> unit m
end

module type TermModel = EffectfulTermModel with type 'a m := 'a

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
    | _ -> weaken (n - 1) @@ Subst.ext (Subst.cmp sb Subst.wk) (Var 0)

  let rec subst (t, sb) =
    match Subst.out sb, t with
    | Subst.Id, _ -> t
    | _, Var i -> proj sb i
    | _, In tf -> In (S.map (fun i a -> subst (a, weaken i sb)) tf)

  and proj sb ix =
    match Subst.out sb with
    | Subst.Id -> Var ix
    | Subst.Cmp (sb1, sb0) -> subst (proj sb0 ix, sb1)
    | Subst.Ext (_, t) -> if ix = 0 then t else proj sb (ix - 1)
    | Subst.Wk -> Var (ix + 1)


  let rec pp fmt t =
    match t with
    | Var i -> Fmt.pf fmt "#%i" i
    | In tf -> S.pp pp fmt tf
end
