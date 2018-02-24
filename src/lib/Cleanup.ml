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
    match sb, t with
    | Id, _ -> t
    | _, Var i -> proj sb i
    | _, In tf -> In (S.map (fun i a -> subst @@ Clo (a, weaken i sb)) tf)

  and proj sb ix =
    match sb with
    | Id -> Var ix
    | Cmp (sb1, sb0) -> subst @@ Clo (proj sb0 ix, sb1)
    | Ext (_, t) -> if ix = 0 then t else proj sb (ix - 1)
    | Wk -> Var (ix + 1)
end

module type ProofState =
sig
  type key

  type ('t, 'a) t
  val return : 'a -> ('t, 'a) t
  val bind : ('t, 'a) t -> ('a -> ('t, 'b) t) -> ('t, 'b) t

  val find : key -> ('t, 'a) t
end

(* This is a model with references to holes *)
module Disect (M : ProofState) (S : Signature) :
sig
  include Model with type 'a f = 'a S.t

  type hole = Ask | Ret of t
  type jdg = {cx : t list; ty : t; hole : hole}
  type 'a m = (jdg, 'a) M.t

  val hole : M.key -> t

  val out : t -> [`F of t f | `V of int] m
end =
struct
  type 'a f = 'a S.t

  type t =
    | Var of int
    | In of t S.t
    | Ref of [`Defer of M.key * t subst | `Done of t] ref
  (* Wrapping the above in a reference to a sum lets me avoid
     having to destructively update the environment in order to
     make updates that memoize lookup-and-subst operations; these
     are different from other updates to the environment in that they
     contain no change in information. Better to deal with it locally! *)

  type hole = Ask | Ret of t
  type jdg = {cx : t list; ty : t; hole : hole}
  type 'a m = (jdg, 'a) M.t

  let var i = Var i

  let into tf = In tf

  let rec weaken n sb =
    match n with
    | 0 -> sb
    | _ -> weaken (n - 1) @@ Ext (Cmp (sb, Wk), Var 0)

  let rec subst (Clo (t, sb)) =
    match sb, t with
    | Id, _ -> t
    | _, Var i -> proj sb i
    | _, In tf -> In (S.map (fun i a -> subst @@ Clo (a, weaken i sb)) tf)
    | _, Ref r ->
      match !r with
      | `Defer (key, sb') -> Ref (ref @@ `Defer (key, Cmp (sb, sb')))
      | `Done t -> subst @@ Clo (t, sb)

  and proj sb ix =
    match sb with
    | Id -> Var ix
    | Cmp (sb1, sb0) -> subst @@ Clo (proj sb0 ix, sb1)
    | Ext (_, t) -> if ix = 0 then t else proj sb (ix - 1)
    | Wk -> Var (ix + 1)

  let rec out t =
    match t with
    | Var i -> M.return @@ `V i
    | In tf -> M.return @@ `F tf
    | Ref r ->
      match !r with
      | `Done t -> out t
      | `Defer (key, sb) ->
        M.bind (M.find key) @@ fun {hole;_} ->
          match hole with
          | Ask -> failwith "[out]: got Ask"
          | Ret t ->
            let t' = subst @@ Clo (t, sb) in
            r := `Done t';
            out t'


  let hole key =
    Ref (ref @@ `Defer (key, Id))

end
