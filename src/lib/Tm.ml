module Idx = struct
  type t = Mk of int
  [@@deriving (eq, ord, show)]
end

module Bind = struct
  type 'a t = Mk of 'a
  [@@deriving (eq, ord, show)]
end

(* TODO:
   - coe: coercion across an equation
   - hcom: composition of equations
   - join: equate boundary-compatible equations
*)

module rec Chk : sig
  type t =
    | Up of Inf.t
    | Bool
    | Pi of Chk.t * Chk.t Bind.t
    | Sg of Chk.t * Chk.t Bind.t
    | Eq of Chk.t Bind.t * Chk.t * Chk.t
    | Lam of Chk.t Bind.t
    | Pair of Chk.t * Chk.t
    | Tt
    | Ff
    | Dim0
    | Dim1
    | U
  [@@deriving (eq, ord, show)]
end =
struct
  type t =
    | Up of Inf.t
    | Bool
    | Pi of Chk.t * Chk.t Bind.t
    | Sg of Chk.t * Chk.t Bind.t
    | Eq of Chk.t Bind.t * Chk.t * Chk.t
    | Lam of Chk.t Bind.t
    | Pair of Chk.t * Chk.t
    | Tt
    | Ff
    | Dim0
    | Dim1
    | U
  [@@deriving (eq, ord, show)]
end

and Inf : sig
  type t =
    | V of Idx.t
    | App of Inf.t * Chk.t
    | Proj1 of Inf.t
    | Proj2 of Inf.t
    | If of Chk.t Bind.t * Inf.t * Chk.t * Chk.t
    | Down of Chk.t * Chk.t
  [@@deriving (eq, ord, show)]
end =
struct
  type t =
    | V of Idx.t
    | App of Inf.t * Chk.t
    | Proj1 of Inf.t
    | Proj2 of Inf.t
    | If of Chk.t Bind.t * Inf.t * Chk.t * Chk.t
    | Down of Chk.t * Chk.t
  [@@deriving (eq, ord, show)]
end
