module type EnvMonad =
sig
  type key

  (* Semantically, 'jdg should be a poset; we refer to its order as the "information order". *)
  type ('a, 'jdg) t


  (* This is intended to be abstract. Sometimes to interface with other code outside the monad,
     we need a way to turn a monadic action into a value, inside the monad. This is helpful
     when dealing with higher order functions, such as in the case of pretty-printers. *)
  type 'jdg env
  val get_env : ('jdg env, 'jdg) t
  val run : 'jdg env -> ('a, 'jdg) t -> 'a


  val find : key -> ('a, 'jdg) t
  val alloc : 'jdg -> (key, 'a) t

  (* INVARIANT: the update must be monotone in the sense of the information order on 'jdg.
     Behavior is UNDEFINED when the update is not an improvement. *)
  val improve : key -> 'jdg -> (unit, 'a) t

  include Monad.S2 with type ('a, 'jdg) t := ('a, 'jdg) t
end
