module Bind = struct
  type 'a t = Mk of 'a
  [@@deriving (eq, ord, show)]
end

(* TODO:
   - coe: coercion across an equation
   - hcom: composition of equations
   - join: equate boundary-compatible equations
*)

(* We define terms in explicit substitutions style; this is essential for an efficient evaluator
   and an efficient typechecker. *)
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
  | Sub of chk * sub
  [@@deriving (eq, ord, show)]
and inf =
  | Var
  | App of inf * chk
  | Proj1 of inf
  | Proj2 of inf
  | If of chk Bind.t * inf * chk * chk
  (* | Coe of (chk * chk) * chk Bind.t * chk *)
  | Down of chk * chk
  [@@deriving (eq, ord, show)]
and sub =
  | Id
  | Wk
  | Cmp of sub * sub
  | Ext of sub * chk
  [@@deriving (eq, ord, show)]
