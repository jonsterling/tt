type idx = Idx of int
type 'a bind = B of 'a

(* TODO:
   - coe: coercion across an equation
   - hcom: composition of equations
   - join: equate boundary-compatible equations
*)

type chk = 
  | Up of inf
  | Bool
  | Pi of chk * chk bind
  | Sg of chk * chk bind
  | Eq of chk bind * chk * chk
  | Lam of chk bind
  | Pair of chk * chk
  | Tt
  | Ff
  | Dim0
  | Dim1
  | U
and inf = 
  | V of idx
  | App of inf * chk
  | Proj1 of inf
  | Proj2 of inf
  | If of chk bind * inf * chk * chk
  | Down of chk * chk
