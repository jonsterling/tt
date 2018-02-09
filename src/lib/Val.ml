type d =
  | Clo of Tm.chk Tm.Bind.t * env
  | Up of d * dne
  | Bool
  | Pi of d * d
  | Sg of d * d
  | Pair of d * d

and dne =
  | Atom of int
  | App of dne * dnf
  | Proj1 of dne
  | Proj2 of dne

and dnf =
  | Down of d * d

and env = d list
