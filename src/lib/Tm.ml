module Bind = struct
  type 'a t = Mk of 'a
  [@@deriving (eq, ord, show)]
end

(* TODO:
   - coe: coercion across an equation
   - hcom: composition of equations
   - join: equate boundary-compatible equations
*)

type chk =
  | Up of inf
  | Bool
  | Pi of chk * chk Bind.t
  | Sg of chk * chk Bind.t
  | Eq of chk Bind.t * chk * chk
  | Lam of chk Bind.t
  | Pair of chk * chk
  | Tt
  | Ff
  | Dim0
  | Dim1
  | U
  | Sub of chk * subst
  [@@deriving (eq, ord, show)]
and inf =
  | Var
  | App of inf * chk
  | Proj1 of inf
  | Proj2 of inf
  | If of chk Bind.t * inf * chk * chk
  | Coe of (chk * chk) * chk Bind.t * chk
  | Down of chk * chk
  [@@deriving (eq, ord, show)]
and subst =
  | Id
  | Wk
  | Cmp of subst * subst
  | Ext of subst * chk
