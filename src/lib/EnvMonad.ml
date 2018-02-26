module type EnvMonad = sig
  module Key : sig
    type t
    [@@deriving (compare, sexp, show)]
  end

  module T : sig
    (* Semantically, 'jdg should be a poset; we refer to its order as the "information order". *)
    type ('a, 'jdg) t
    [@@deriving (compare, sexp, show)]
  end

  module Env : sig
    (* This is intended to be abstract. Sometimes to interface with other code outside the monad,
       we need a way to turn a monadic action into a value, inside the monad. This is helpful
       when dealing with higher order functions, such as in the case of pretty-printers. *)
    type 'jdg t
    [@@deriving (compare, sexp, show)]
  end

  val get_env : ('jdg Env.t, 'jdg) T.t

  val run : 'jdg Env.t -> ('a, 'jdg) T.t -> 'a

  val find : Key.t -> ('a, 'jdg) T.t

  val alloc : 'jdg -> (Key.t, 'a) T.t

  (* INVARIANT: the update must be monotone in the sense of the information order on 'jdg.
     Behavior is UNDEFINED when the update is not an improvement. *)
  val improve : Key.t -> 'jdg -> (unit, 'a) T.t

  include Monad.S2
    with type ('a, 'jdg) t := ('a, 'jdg) T.t
end
