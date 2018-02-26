type ('a, 's) f =
  | Id
  | Wk
  | Cmp of 's * 's
  | Ext of 's * 'a
[@@deriving (compare, hash, sexp, show)]

type 'a t =
  | In of ('a, 'a t) f
[@@deriving (compare, hash, sexp, show)]

type ('a, 'b) tensor = 'a * 'b t
[@@deriving (compare, sexp, show)]

let out s =
  let In sf = s in
  sf

let into sf = In sf

let id = into Id

let wk = into Wk

let cmp s2 s1 = into @@ Cmp (s2, s1)

let ext s t = into @@ Ext (s, t)
