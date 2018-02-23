open Sigs

module ElabCore : ElabCore

module Elab (E : ElabCore) :
sig
  include ElabCore

  val ask : ctx:Tm.term ctx -> ty:Tm.term -> (Tm.hole * Tm.term) t
end
