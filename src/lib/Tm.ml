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
  | Unit
  | Bool
  | Pi of chk * chk Bind.t
  | Sg of chk * chk Bind.t
  | Eq of chk Bind.t * chk * chk
  | Lam of chk Bind.t
  | Pair of chk * chk
  | Ax
  | Tt
  | Ff
  | Dim0
  | Dim1
  | U
  | ChkSub of chk * sub
  | Hole of chk * chk Bind.t
  | Guess of chk * chk * chk Bind.t
  [@@deriving (eq, ord, show)]
and inf =
  | Var
  | App of inf * chk
  | Proj1 of inf
  | Proj2 of inf
  | If of chk Bind.t * inf * chk * chk
  | Down of chk * chk
  | InfSub of inf * sub
  [@@deriving (eq, ord, show)]
and sub =
  | Id
  | Wk
  | Cmp of sub * sub
  | Ext of sub * chk
  [@@deriving (eq, ord, show)]

type ctx =
  | CNil
  | CExt of ctx * chk


let rec weak n =
  match n with
  | 0 -> Wk
  | n -> Cmp (weak (n - 1), Wk)

let var i =
  match i with
  | 0 -> Var
  | n -> InfSub (Var, weak (n - 1))
