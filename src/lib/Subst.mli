module F : sig
  type ('a, 's) t =
    | Id
    | Wk
    | Cmp of 's * 's
    | Ext of 's * 'a
  [@@deriving (compare, hash, sexp, show)]
end

module T : sig
  type 'a t
  [@@deriving (compare, hash, sexp, show)]
end

module Tensor : sig
  type ('a, 'b) t = 'a * 'b T.t
  [@@deriving (compare, hash, sexp, show)]
end

val into : ('a, 'a T.t) F.t -> 'a T.t

val out : 'a T.t -> ('a, 'a T.t) F.t

val id : 'a T.t

val wk : 'a T.t

val cmp : 'a T.t -> 'a T.t -> 'a T.t

val ext : 'a T.t -> 'a -> 'a T.t
