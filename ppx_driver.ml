(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* A basic ppx driver, which dynlinks AST rewriters specified on its
   command-line and run all of them.  All command-line arguments
   ending with .cmo, .cma or .cmxs are assumed to refer to compiled
   plugins, ready to be dynlinked, and which are supposed to register
   rewriters through [Ast_mapper.register].  Each rewriter in run
   once.  All other arguments on the command-line are passed to the
   preceeding plugin.  *)

let current = ref None

let () =
  at_exit
    (fun () ->
      match !current with
      | None -> ()
      | Some (cmd, name) ->
          Printf.eprintf "(in rewriter '%s' loaded from %s)\n%!" name cmd
    )

let run_sub cmd name f =
  current := Some (cmd, name);
  let res =
    try f ()
    with exn -> prerr_endline (Printexc.to_string exn); exit 2
  in
  current := None;
  res

let is_plugin s =
  Filename.check_suffix s ".cmo" ||
  Filename.check_suffix s ".cma" ||
  Filename.check_suffix s ".cmxs"

let rec parse_cmdline = function
  | [] -> []
  | cmd :: rest when is_plugin cmd ->
      let cmd = Dynlink.adapt_filename cmd in
      let rec parse_args args = function
        | arg :: rest when not (is_plugin arg) ->
            parse_args (arg :: args) rest
        | rest ->
            (cmd, List.rev args) :: parse_cmdline rest
      in
      parse_args [] rest
  | arg :: _ ->
      Printf.eprintf "Don't know what to do with command-line argument '%s'.\n%!" arg;
      exit 2

let load_plugin mappers (cmd, args) =
  Ast_mapper.register_function :=
    (fun name f ->
      run_sub cmd name
        (fun () -> mappers := (cmd, name, f args) :: !mappers)
    );
  try
    Dynlink.loadfile cmd
  with exn ->
    Printf.eprintf "Error while loading %s:\n%s\n" cmd (Printexc.to_string exn);
    exit 2

let main args =
  let mappers = ref [] in
  List.iter (load_plugin mappers) (parse_cmdline args);
  let mappers = List.rev !mappers in
  let apply_all r f s =
    List.fold_left
      (fun (f, s) (cmd, name, mapper) ->
        run_sub cmd name (fun () -> r mapper f s)
      )
      (f, s)
      mappers
  in
  object
    method implementation = apply_all (fun m -> m # implementation)
    method interface = apply_all (fun m -> m # interface)
  end

let () =
  Printexc.register_printer
    (function
      | Dynlink.Error err -> Some (Dynlink.error_message err)
      | _ -> None
    )

let () = Ast_mapper.run_main main

