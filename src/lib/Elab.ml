open Sigs

module Types =
struct
  type hole = string

  (* Here is where things like hash keys and tags should be kept *)
  type term = 
    | In of (int, term, subst) term_f
    | Ref of hole * subst
  and subst = InSb of (term, subst) subst_f
end

module Tm = 
struct
  type hole = Types.hole
  type term = Types.term
  type subst = Types.subst

  open Types

  let meta alpha sb = 
    Ref (alpha, sb)

  let into tf = In tf

  let intoS sbf = InSb sbf


  (* TODO: implement *)
  let subst ~sb ~tm : term =
    match tm with 
    | In tmf ->
      begin match tmf with
      | Var _ -> failwith ""
      | Lam _ -> failwith ""
      | App (_, _) -> failwith ""
      | Ax -> tm
      | Pi (_, _) -> failwith ""
      | Unit -> tm
      | Univ -> tm
      end
    | Ref (key, sb') -> Ref (key, intoS @@ Cmp (sb, sb'))
end

module Elab : Elab 
  with type term = Tm.term
  and type subst = Tm.subst
  and type hole = Tm.hole
= 
struct
  type hole = Tm.hole
  type term = Types.term
  type subst = Types.subst

  module HT = Caml.Hashtbl.Make (String)

  type env = term jdg HT.t

  type 'a t = env -> 'a

  let bind m ~f rho = f (m rho) rho
  let return a _ = a
  let map = `Define_using_bind

  let rec alt (ms : 'a t list) : 'a t =
    match ms with 
    | [] -> failwith "No alternatives"
    | m::ms ->
      fun env -> 
        let env' = HT.copy env in
        try m env with 
        | _ -> alt ms env'

  let alloc jdg env =
    let key = "fresh" in (* TODO *)
    HT.add env key jdg;
    key

  module Let_syntax = 
  struct
    let bind = bind
  end

  let ask ~ctx ~ty =
    let%bind key = alloc @@ Chk (ctx, Ask, ty) in
    return (key, Tm.meta key @@ Tm.intoS Id)
  
  let find key env =
    HT.find env key

  (* TODO: 
     - [ ] possibly typecheck the term
     - [ ] allow things other than Ask via unification
   *)
  let fill key tm env =
    match HT.find env key with
    | Chk (ctx, Ask, ty) -> HT.replace env key @@ Chk (ctx, Ret tm, ty) 
    | _ -> failwith ""

  let rec out t : (int, term, subst) term_f t =
    match t with 
    | Types.In tf -> return tf
    | Types.Ref (key, sub) ->
      match%bind find key with
      | Chk (_, Ret t', _) -> out @@ Tm.subst ~sb:sub ~tm:t'
      | Chk (_, Ask, _) -> failwith "[out]: Term is a hole"
end
