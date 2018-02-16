(* TODO *)
type hole = string

module Ref :
sig
  include Map.OrderedType
  val fresh : unit -> t
end =
struct
  type t = int

  let global = ref 0

  let fresh () =
    let i = !global in
    global := i + 1;
    i

  let compare (i : int) (j : int) =
    compare i j
end

(* Just an example. We'll need a type to serve as the signature endofunctor to the language. *)
module TmF =
struct
  type 'a t =
    | Var of int
    | Lam of 'a
    | App of 'a * 'a
    | Ax
end

module Dev =
struct
  type 'a t =
    | Ask of 'a * 'a
    | Guess of 'a * 'a
    | Ret of 'a
end

module Disect (F : sig type 'a t end) =
struct
  type t =
    | In of t F.t
    | Ref of Ref.t
end

module Tm = Disect (TmF)
module Env = Map.Make (Ref)

type env = Tm.t Dev.t Env.t

(* TODO: design a monad *)

let attack alpha (e : env) =
  match Env.find alpha e with
  | Dev.Ask (ty, bdy) ->
    let beta = Ref.fresh () in
    let ask = Dev.Ask (ty, Tm.In (TmF.Var 0)) in
    let guess = Dev.Guess (Tm.Ref beta, bdy) in
    Env.add alpha guess @@ Env.add beta ask e
  | _ -> failwith "attack"
