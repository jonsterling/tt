 type ('var, 'term, 'subst) term_f = 
  | Var of 'var
  | Lam of 'term
  | App of 'term * 'term
  | Ax
  | Pi of 'term * 'term
  | Unit
  | Univ

type ('term, 'subst) subst_f = 
  | Id
  | Wk
  | Cmp of 'subst * 'subst
  | Ext of 'subst * 'term

type 'a subject = 
  | Ret of 'a
  | Ask

type 'a ctx = 
  | CNil
  | CExt of 'a ctx * 'a

type 'a jdg = 
  | Chk of 'a ctx * 'a subject * 'a


module type Tm = 
sig
  type hole


  (* New Line: this monad handles proof state, but things like sharing and hash consing should probably be 
    separate and imperative. *)
  module M :
  sig
    type 'a m
    val bind : 'a m -> f:('a -> 'b m) -> 'b m
    val ret : 'a -> 'a m
  end

  type term
  type subst

  val alloc : term jdg -> hole M.m
  val ask : term ctx -> term -> term M.m

  val find : hole -> term jdg M.m
  val fill : hole -> term -> unit M.m

  val subst : subst -> term -> term
  val out : term -> (int, term, subst) term_f M.m

  val into : (int, term, subst) term_f -> term
  val intoS : (term, subst) subst_f -> subst

  val meta : hole -> subst -> term 
end


module Tm : Tm =
struct
  type hole = string

  module Env = Caml.Map.Make (String)

  (* TODO: here is where you would store things like hash keys and tags *)
  type term = 
    | In of (int, term, subst) term_f
    | Ref of string * subst
  and subst = InSb of (term, subst) subst_f

  type env = term jdg Env.t

  module M = 
  struct
    type 'a m = env -> 'a * env
    let ret a rho = (a, rho)

    let bind _ ~f:_ = failwith ""
  end

  module Let_syntax = 
  struct
    let bind = M.bind
  end

  (* TODO: most of the following operations should be memoizable, but it may be a little tricky since 
     the identity of an instance of one of these operations includes the identity of the current state. 
     Ideas?
  *)

  let subst _ _ = failwith "todo"
  let out _ = failwith "todo"
  let into _ = failwith "todo"
  let intoS _ = failwith "todo"

  let meta alpha sb =
    Ref (alpha, sb)

  let alloc _ = failwith "todo"

  let ask cx ty = 
    let%bind alpha = alloc @@ Chk (cx, Ask, ty) in
    meta alpha (intoS Id)

  let find _ = failwith "todo"

  let fill _ = failwith "todo"
end

module Let_syntax = 
struct
  let bind = Tm.M.bind
end

let pi alpha = 
  match%bind Tm.find alpha with 
  | Chk (cx, Ask, ty) ->
    begin match%bind Tm.out ty with 
    | Univ ->
      let%bind dom = Tm.ask cx ty in
      let%bind cod = Tm.ask (CExt (cx, dom)) ty in
      Tm.fill alpha @@ Tm.into @@ Pi (dom, cod)
    | _ -> failwith ""
    end
  | _ -> failwith ""

let lambda alpha =
  match%bind Tm.find alpha with 
  | Chk (gm, Ask, ty) ->
    begin match%bind Tm.out ty with
    | Pi (dom, cod) ->
      let%bind bdy = Tm.ask (CExt (gm, dom)) cod in
      Tm.fill alpha @@ Tm.into @@ Lam bdy
    | _ -> failwith ""
    end
  | _ -> failwith ""