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
