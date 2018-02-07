module Lvl = struct
  type t = Mk of int
  [@@deriving (eq, ord, show)]
end

module rec NF : sig
  type t =
    | Lam of Clo.t
    | Pair of NF.t * NF.t
    | Pi of NF.t * Clo.t
    | Sg of NF.t * Clo.t
    | Eq of Clo.t * NF.t * NF.t
    | Bool
    | Tt
    | Ff
    | U
    | EDim
    | Dim0
    | Dim1
    | Up of Neu.t Ann.t
  [@@deriving (eq, ord, show)]
end = NF

and Neu : sig
  type t =
    | Atom of Lvl.t
    | Proj1 of Neu.t
    | Proj2 of Neu.t
    | App of Neu.t * NF.t Ann.t
    | If of Clo.t * Neu.t * NF.t * NF.t
  [@@deriving (eq, ord, show)]
end = Neu

and Env : sig
  type t = NF.t list
  [@@deriving (eq, ord, show)]
end = Env

and Ann : sig
  type 'a t = Mk of 'a * NF.t
  [@@deriving (eq, ord, show)]
end = Ann

and Clo : sig
   type t = Mk of Tm.Chk.t Tm.Bind.t * Env.t
   [@@deriving (eq, ord, show)]
end = Clo
