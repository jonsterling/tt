module type EnvMonad =
sig
  type key

  (* Semantically, 'jdg should be a poset; we refer to its order as the "information order". *)
  type ('jdg, 'a) t

  val return : 'a -> ('jdg, 'a) t
  val bind : ('jdg, 'a) t -> ('a -> ('jdg, 'b) t) -> ('jdg, 'b) t

  (* This is intended to be abstract. Sometimes to interface with other code outside the monad,
     we need a way to turn a monadic action into a value, inside the monad. This is helpful
     when dealing with higher order functions, such as in the case of pretty-printers. *)
  type 'jdg env
  val get_env : ('jdg, 'jdg env) t
  val run : 'jdg env -> ('jdg, 'a) t -> 'a


  val find : key -> ('jdg, 'a) t
  val alloc : 'jdg -> ('a, key) t

  (* INVARIANT: the update must be monotone in the sense of the information order on 'jdg.
     Behavior is UNDEFINED when the update is not an improvement. *)
  val improve : key -> 'jdg -> ('a, unit) t
end
