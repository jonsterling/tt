open Signature
open Model
open EnvMonad

module type ProofState =
sig
  module Env : EnvMonad

  type t

  (* A subject is Ask if it has not been refined yet; it is Ret if it has been refined.
     The information order is that [Ask <= Ret t]. *)
  type subject =
    | Ask
    | Ret of t

  type jdg = {cx : t list; ty : t; hole : subject}
  type 'a term_f
  type 'a m = ('a, jdg) Env.t

  include EffectfulTermModel
    with type 'a f := 'a term_f
     and type 'a m := 'a m
     and type t := t


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
    | Ref of [`Defer of (M.key, t) Subst.tensor | `Done of t] ref
    (* Wrapping the above in a reference to a sum lets me avoid
       having to destructively update the environment in order to
       make updates that memoize lookup-and-subst operations; these
       are different from other updates to the environment in that they
       contain no change in information. Better to deal with it locally! *)

  type subject = Ask | Ret of t
  type jdg = {cx : t list; ty : t; hole : subject}
  type 'a m = ('a, jdg) M.t

  let var i = Var i

  let into tf = In tf

  let rec weaken n sb =
    match n with
    | 0 -> sb
    | _ -> weaken (n - 1) @@ Subst.ext (Subst.cmp sb Subst.wk) (Var 0)

  let rec subst (t, sb) =
    match Subst.out sb, t with
    | Subst.Id, _ -> t
    | _, Var i -> proj sb i
    | _, In tf -> In (S.map (fun i a -> subst (a, weaken i sb)) tf)
    | _, Ref r ->
      match !r with
      | `Defer (key, sb') -> Ref (ref @@ `Defer (key, Subst.cmp sb sb'))
      | `Done t -> subst (t, sb)

  and proj sb ix =
    match Subst.out sb with
    | Subst.Id -> Var ix
    | Subst.Cmp (sb1, sb0) -> subst (proj sb0 ix, sb1)
    | Subst.Ext (_, t) -> if ix = 0 then t else proj sb (ix - 1)
    | Subst.Wk -> Var (ix + 1)

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
        M.bind (M.find key) ~f:begin fun {hole; _} ->
          match hole with
          | Ask -> failwith "[out]: got Ask"
          | Ret t ->
            let t' = subst (t, sb) in
            r := `Done t';
            out t'
        end

  let rec pp fmt t =
    M.bind (out t) ~f:begin fun tf ->
      match tf with
      | `V i -> M.return @@ Fmt.pf fmt "#%i" i
      | `F tf ->
        M.bind M.get_env ~f:begin fun env ->
          M.return @@
          S.pp (fun fmt t -> M.run env (pp fmt t)) fmt tf
        end
    end

  let hole key =
    Ref (ref @@ `Defer (key, Subst.id))

end
