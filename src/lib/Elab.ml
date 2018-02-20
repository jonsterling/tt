open Sigs

module Types =
struct
  type hole = string

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
  let subst ~sb:_ ~tm:_ = failwith ""
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

  let fill = failwith ""  

  let rec out t =
    match t with 
    | Types.In tf -> return tf
    | Types.Ref (key, sub) ->
      let%bind t' = find key in
      out @@ Tm.subst ~sb:sub ~tm:t'
end
