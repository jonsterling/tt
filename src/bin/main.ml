(* This is an experiment in designing a formal type theory inspired by
   both Observational Type Theory and American Cubical Type Theory.

   From OTT we take the idea of extensional, proof-irrelevant heterogeneous
   equality in a decidable setting, for an ergonomic proof theory;
   from (American) Cubical Type Theory, we take the idea of using abstract
   dimensions to realize this structure, and the specific coe and hcom
   operations.

   We will add a third operation, "join", which equates any two equations which
   have compatible bounadary.
*)

open Cmdliner

let cmd_help: unit Lwt.t Term.t * Term.info =
  let doc = "show help" in
  Term.
    ( ret @@ pure @@ `Help ( `Pager, None )
    , info "help" ~doc
    )

let cmd_default =
  Term.
    ( ret @@ pure @@ `Help ( `Pager, None )
    , info "tt" ~version:"0.0.0"
    )

let cmds = [
  cmd_help;
]

let main () =
  match Term.eval_choice cmd_default cmds with
  | `Error e -> exit 1
  | `Ok expr -> Lwt_main.run expr
  | _ -> exit 0

let () =
  if not !Sys.interactive then
    main ()
