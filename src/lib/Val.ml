module Lvl = struct
  type t = Mk of int
  [@@deriving (eq, ord, show)]
end

type nf =
  | Lam of clo
  | Pair of nf * nf
  | Pi of nf * clo
  | Sg of nf * clo
  | Eq of clo * nf * nf
  | Bool
  | Tt
  | Ff
  | U
  | EDim
  | Dim0
  | Dim1
  | Up of neu ann
  [@@deriving (eq, ord, show)]

and neu =
  | Atom of Lvl.t
  | Proj1 of neu
  | Proj2 of neu
  | App of neu * nf ann
  | If of clo * neu * nf * nf
  [@@deriving (eq, ord, show)]

and env = nf list [@@deriving (eq, ord, show)]
and 'a ann = Ann of 'a * nf [@@deriving (eq, ord, show)]
and clo = Clo of Tm.chk Tm.Bind.t * env [@@deriving (eq, ord, show)]