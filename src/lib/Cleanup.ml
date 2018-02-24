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

  val pp : ih:('a Fmt.t) -> 'a t Fmt.t
end

module type Model =
sig
  type 'a f

  type t
  val var : int -> t
  val into : t f -> t
  val subst : (t, t) tensor -> t
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


  let rec pp fmt t =
    match t with
    | Var i -> Fmt.pf fmt "#%i" i
    | In tf -> S.pp pp fmt tf
end


module type EnvMonad =
sig
  type key

  (* Semantically, 't should be a poset; we refer to its order as the "information order". *)
  type ('t, 'a) t

  val return : 'a -> ('t, 'a) t
  val bind : ('t, 'a) t -> ('a -> ('t, 'b) t) -> ('t, 'b) t

  (* This is intended to be abstract. Sometimes to interface with other code outside the monad,
     we need a way to turn a monadic action into a value, inside the monad. This is helpful
     when dealing with higher order functions, such as in the case of pretty-printers. *)
  type 't env
  val get_env : ('t, 't env) t
  val run : 't env -> ('t, 'a) t -> 'a


  val find : key -> ('t, 'a) t
  val alloc : key -> 't -> ('a, unit) t

  (* INVARIANT: the update must be monotone in the sense of the information order on 't.
     Behavior is UNDEFINED when the update is not an improvement. *)
  val improve : key -> 't -> ('a, unit) t
end

module type ProofState =
sig
  module Env : EnvMonad

  type jdg
  type 'a term_f
  type 'a m = (jdg, 'a) Env.t

  include EffectfulTermModel
    with type 'a f := 'a term_f
     and type 'a m := 'a m

  (* A subject is Ask if it has not been refined yet; it is Ret if it has been refined.
     The information order is that [Ask <= Ret t]. *)
  type subject =
    | Ask
    | Ret of t

  val hole : Env.key -> t
  val out : t -> [`F of t term_f | `V of int] m
end


(* This is a model with references to holes *)
module ProofState (M : EnvMonad) (S : Signature) :
  ProofState
  with module Env = M
   and type 'a term_f = 'a S.t
=
struct
  module Env = M
  type 'a term_f = 'a S.t

  type t =
    | Var of int
    | In of t S.t
    | Ref of [`Defer of M.key * t subst | `Done of t] ref
    (* Wrapping the above in a reference to a sum lets me avoid
       having to destructively update the environment in order to
       make updates that memoize lookup-and-subst operations; these
       are different from other updates to the environment in that they
       contain no change in information. Better to deal with it locally! *)

  type subject = Ask | Ret of t
  type jdg = {cx : t list; ty : t; hole : subject}
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

  (* The clever bit is that when we hit a reference into the proof state,
     we look it up and perform its associated deferred substitution; then
     we destructively update the reference accordingly. This is justified
     because we require as an invariant that updates to the proof state be
     monotone. *)
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

  let rec pp fmt t =
    M.bind (out t) @@ fun tf ->
    match tf with
    | `V i -> M.return @@ Fmt.pf fmt "#%i" i
    | `F tf ->
      M.bind M.get_env @@ fun env ->
      M.return @@
      S.pp (fun fmt t -> M.run env (pp fmt t)) fmt tf

  let hole key =
    Ref (ref @@ `Defer (key, Id))

end
