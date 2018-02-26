type ('a, 's) f =
  | Id
  | Wk
  | Cmp of 's * 's
  | Ext of 's * 'a

type 'a t

val into : ('a, 'a t) f -> 'a t

val out : 'a t -> ('a, 'a t) f

val id : 'a t

val wk : 'a t

val cmp : 'a t -> 'a t -> 'a t

val ext : 'a t -> 'a -> 'a t

type ('a, 'b) tensor = 'a * 'b t
