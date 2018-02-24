type 'a t =
  | Id
  | Wk
  | Cmp of 'a t * 'a t
  | Ext of 'a t * 'a

type ('a, 'b) tensor = 'a * 'b t
