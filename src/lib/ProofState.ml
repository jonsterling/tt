open Signature
open EnvMonad

(* This is a model with references to holes *)
module ProofState (Mon : EnvMonad) (Sig : Signature) = struct
  module T = struct
    type t =
      | Var of int
      | In of t Sig.t
      | Ref of [`Defer of (Mon.Key.t, t) Subst.Tensor.t | `Done of t] ref
    [@@deriving (compare, sexp, show)]
    (* Wrapping the above in a reference to a sum lets me avoid
       having to destructively update the environment in order to
       make updates that memoize lookup-and-subst operations; these
       are different from other updates to the environment in that they
       contain no change in information. Better to deal with it locally! *)
  end

  module Subject = struct
    type t =
      | Ask
      | Ret of T.t
    [@@deriving (compare, sexp, show)]
  end

  module Jdg = struct
    type t = {
      cx : T.t list;
      ty : T.t;
      hole : Subject.t;
    }
    [@@deriving (compare, sexp, show)]
  end

  module TermF = struct
    type 'a t = 'a Sig.t
    [@@deriving (compare, sexp, show)]
  end

  module M = struct
    type 'a t = ('a, Jdg.t) Mon.T.t
    [@@deriving (compare, sexp, show)]
  end

  let var i = T.Var i

  let into tf = T.In tf

  let rec weaken n sb =
    match n with
    | 0 -> sb
    | _ -> weaken (n - 1) @@ Subst.ext (Subst.cmp sb Subst.wk) (T.Var 0)

  let rec subst (t, sb) =
    match Subst.out sb, t with
    | Subst.F.Id, _ -> t
    | _, T.Var i -> proj sb i
    | _, T.In tf -> T.In (Sig.map (fun i a -> subst (a, weaken i sb)) tf)
    | _, T.Ref r ->
      match !r with
      | `Defer (key, sb') -> T.Ref (ref @@ `Defer (key, Subst.cmp sb sb'))
      | `Done t -> subst (t, sb)

  and proj sb ix =
    match Subst.out sb with
    | Subst.F.Id -> T.Var ix
    | Subst.F.Cmp (sb1, sb0) -> subst (proj sb0 ix, sb1)
    | Subst.F.Ext (_, t) -> if ix = 0 then t else proj sb (ix - 1)
    | Subst.F.Wk -> T.Var (ix + 1)

  (* The clever bit is that when we hit a reference into the proof state,
     we look it up and perform its associated deferred substitution; then
     we destructively update the reference accordingly. This is justified
     because we require as an invariant that updates to the proof state be
     monotone. *)
  let rec out t =
    match t with
    | T.Var i -> Mon.return @@ `V i
    | T.In tf -> Mon.return @@ `F tf
    | T.Ref r ->
      match !r with
      | `Done t -> out t
      | `Defer (key, sb) ->
        Mon.bind (Mon.find key) ~f:begin fun Jdg.{hole} ->
          match hole with
          | Subject.Ask -> failwith "[out]: got Ask"
          | Subject.Ret t ->
            let t' = subst (t, sb) in
            r := `Done t';
            out t'
        end

  let rec pretty fmt t =
    Mon.bind (out t) ~f:begin fun tf ->
      match tf with
      | `V i -> Mon.return @@ Fmt.pf fmt "#%i" i
      | `F tf ->
        Mon.bind Mon.get_env ~f:begin fun env ->
          Mon.return @@
          Sig.pretty (fun fmt t -> Mon.run env (pretty fmt t)) fmt tf
        end
    end

  let hole key =
    T.Ref (ref @@ `Defer (key, Subst.id))
end
