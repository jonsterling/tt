module F = struct
  type ('a, 's) t =
    | Id
    | Wk
    | Cmp of 's * 's
    | Ext of 's * 'a
  [@@deriving (compare, hash, sexp, show)]
end

module T = struct
  type 'a t =
    | In of ('a, 'a t) F.t
  [@@deriving (compare, hash, sexp, show)]
end

module Tensor = struct
  type ('a, 'b) t = 'a * 'b T.t
  [@@deriving (compare, hash, sexp, show)]
end

let out s =
  let T.In sf = s in
  sf

let into sf = T.In sf

let id = into F.Id

let wk = into F.Wk

let cmp s2 s1 = into @@ F.Cmp (s2, s1)

let ext s t = into @@ F.Ext (s, t)
