
(* Thoughts: the 'coe' operator is really a kind of eta expansion, so it needs to be performed
   during readback together with other kinds of eta expansion (for instance, functions are eta
   expanded in the case for Pi in read_nf).
 *)

let rec eval rho t =
  match t with
  | _ -> failwith ""
