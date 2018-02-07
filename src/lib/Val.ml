type lvl = Lvl of int

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
and neu = 
  | Atom of lvl
  | Proj1 of neu
  | Proj2 of neu
  | App of neu * nf ann
  | If of clo * neu * nf * nf
and env = nf list
and 'a ann = Ann of 'a * nf
and clo = Clo of Tm.chk Tm.bind * env