(* The package ppx_drivers is released under the terms of an MIT-like *)
(* license. See the attached LICENSE file.                            *)
(* Copyright 2013 by Alain Frisch and LexiFi.                         *)

(* A basic ppx driver, which dynlinks AST rewriters specified on its
   command-line and run all of them.  All command-line arguments
   ending with .cmo, .cma or .cmxs are assumed to refer to compiled
   plugins, ready to be dynlinked, and which are supposed to register
   rewriters through [Ast_mapper.register].  Each rewriter in run
   once.  All other arguments on the command-line are passed to the
   preceeding plugin.  *)

open Location
open Longident
open Parsetree


let current = ref None
let mappers = ref []
let load_dirs = ref ["."]
let verbose = ref false

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

let plugins = Hashtbl.create 8


let load_plugin cmd args =
  let cmd = Dynlink.adapt_filename cmd in
  let  l =
    try Hashtbl.find plugins cmd
    with Not_found ->
      if !verbose then Printf.printf "ppx_driver: looking for plugin %s ...\n%!" cmd;
      let dir =
        try List.find (fun d -> Sys.file_exists (Filename.concat d cmd)) !load_dirs
        with Not_found ->
          Printf.eprintf "ppx_driver: cannot locate file %s\n%!" cmd;
          exit 2
      in
      if !verbose then Printf.printf "ppx_driver: ... found in %s\n%!" dir;
      let l = ref [] in
      Ast_mapper.register_function :=
        (fun name f ->
          if !verbose then
            Printf.printf "ppx_driver: plugin %s registers rewriter '%s'\n%!" cmd name;
          l := (name, f) :: !l
        );
      begin try Dynlink.loadfile (Filename.concat dir cmd);
      with exn ->
        Printf.eprintf "ppx_driver: error while loading %s:\n%s\n%!" cmd (Printexc.to_string exn);
        exit 2
      end;
      let l = List.rev !l in
      Hashtbl.add plugins cmd l;
      l
  in
  List.iter
    (fun (name, f) ->
      run_sub cmd name
        (fun () -> mappers := (cmd, name, f args) :: !mappers)
    )
    l

let rec parse_cmdline = function
  | [] -> ()
  | "-v" :: rest -> verbose := true; parse_cmdline rest
  | "-I" :: dir :: rest -> load_dirs := dir :: !load_dirs; parse_cmdline rest
  | cmd :: rest when is_plugin cmd ->
      let rec parse_args args = function
        | arg :: rest when not (is_plugin arg) ->
            parse_args (arg :: args) rest
        | rest ->
            load_plugin cmd (List.rev args);
            parse_cmdline rest
      in
      parse_args [] rest
  | arg :: _ ->
      Printf.eprintf "ppx_driver: don't know what to do with command-line argument '%s'.\n%!" arg;
      exit 2


let main args =
  parse_cmdline args;
  let load = function
    | cmd :: args when is_plugin cmd -> load_plugin cmd args
    | l ->
        Printf.eprintf "ppx_driver: cannot parse load_ppx directive %s\n%!" (String.concat "," l)
  in
  let apply_all parse_directive run f s =
    let s =
      List.fold_left
        (fun acc x ->
          match parse_directive x with
          | None -> x :: acc
          | Some args -> load args; acc
        )
        [] s
    in
    List.fold_left
      (fun (f, s) (cmd, name, mapper) ->
        if !verbose then Printf.printf "ppx_driver: running rewriter '%s' registered by %s\n%!" name cmd;
        run_sub cmd name (fun () -> run mapper f s)
      )
      (f, List.rev s)
      (List.rev !mappers)
  in
  let parse_ml = function
    | {pstr_desc=Pstr_primitive ({txt="load_ppx"}, {pval_prim=l})} -> Some l
    | _ -> None
  in
  let parse_mli = function
    | {psig_desc=Psig_value ({txt="load_ppx"}, {pval_prim=l})} when l <> [] -> Some l
    | _ -> None
  in
  object
    method implementation = apply_all parse_ml (fun m -> m # implementation)
    method interface = apply_all parse_mli (fun m -> m # interface)
  end

let () =
  Printexc.register_printer
    (function
      | Dynlink.Error err -> Some (Dynlink.error_message err)
      | _ -> None
    )

let () = Ast_mapper.run_main main

