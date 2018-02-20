open Sigs

module Tm : Tm
module Elab : Elab
  with type term = Tm.term
  and type subst = Tm.subst
  and type hole = Tm.hole