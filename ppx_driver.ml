(* The package ppx_drivers is released under the terms of an MIT-like *)
(* license. See the attached LICENSE file.                            *)
(* Copyright 2013 by Alain Frisch and LexiFi.                         *)

(* A basic ppx driver, which dynlinks AST rewriters specified on its
   command-line and run all of them.  Syntax of the command-line:

     ./ppx_driver [-v] pkg arg ... arg -- pkg arg arg arg ... -- ...

   Each pkg argument refers to an ocamlfind package implementing
   a ppx rewriter.  Each is supposed to register one Parsetree mapper.
   The arg arguments are passed to this mapper.
*)

open Location
open Longident
open Parsetree

let current = ref None
let mappers = ref []
let verbose = ref false

let () = Dynlink.allow_unsafe_modules true

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

let plugins = Hashtbl.create 8
let loaded = Hashtbl.create 8

let () = Findlib.init ()

let preds = [ "byte" ]

let load_plugin cmd args =
  let l =
    try Hashtbl.find plugins cmd
    with Not_found ->
      let pkgs = Findlib.package_deep_ancestors preds [ cmd ] in
      let cmas =
        List.map
          (fun pkg ->
            let dir = Findlib.package_directory pkg in
            try
              let s = Findlib.package_property preds pkg "archive" in
              if s = "ocamlcommon.cma" || s = "findlib.cma" then []
              else [ Filename.concat dir s ]
            with Not_found ->
              []
          ) pkgs
      in
      let cmas = List.flatten cmas in
      let l = ref [] in
      Ast_mapper.register_function :=
        (fun name f ->
          if !verbose then
            Printf.printf "ppx_driver: plugin %s registers rewriter '%s'\n%!" cmd name;
          l := (name, f) :: !l
        );
      let load x =
        if not (Hashtbl.mem loaded x) then begin
          let x = Dynlink.adapt_filename x in
          if !verbose then Printf.printf "ppx_driver: loading %s ...\n%!" x;
          Dynlink.loadfile x;
          Hashtbl.add loaded x ()
        end
      in
      begin try
        List.iter load cmas
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
  | cmd :: rest ->
      let rec parse_args args = function
        | arg :: "--" :: rest ->
            parse_args (arg :: args) rest
        | rest ->
            load_plugin cmd (List.rev args);
            parse_cmdline rest
      in
      parse_args [] rest


let cannot_parse_load_ppx loc =
  Format.eprintf "%a@.ppx_driver: cannot parse load_ppx directive (ignored)@."
    Location.print_error loc

let is_str = function
  | {pexp_desc=Pexp_constant (Const_string (s, _))} -> Some s
  | _ -> None

let to_str e =
  match is_str e with
  | Some s -> s
  | None -> raise Exit

let parse_load_ppx loc = function
  | PStr [ {pstr_desc=Pstr_eval(e, _)} ] ->
    begin match is_str e, e with
    | Some s, _ -> Some [s]
    | None, {pexp_desc=Pexp_apply(e, el)} ->
      begin try Some (to_str e :: List.map (fun (_, e) -> to_str e) el)
      with Exit -> cannot_parse_load_ppx loc; None
      end
    | _ ->  cannot_parse_load_ppx loc; None
    end
  | _ -> cannot_parse_load_ppx loc; None

let main args =
  parse_cmdline args;
  let load loc = function
    | cmd :: args -> load_plugin cmd args
    | [] -> cannot_parse_load_ppx loc
  in
  let apply_all parse_directive run _m s =
    let s =
      List.fold_left
        (fun acc x ->
          match parse_directive x with
          | Some ({txt="load_ppx"; loc}, x) ->
            begin match parse_load_ppx loc x with
            | None -> acc
            | Some l -> load loc l; acc
            end
          | _ -> x :: acc
        )
        [] s
    in
    List.fold_left
      (fun s (cmd, name, mapper) ->
        if !verbose then Printf.printf "ppx_driver: running rewriter '%s' registered by %s\n%!" name cmd;
        run_sub cmd name (fun () -> run mapper s)
      )
      (List.rev s)
      (List.rev !mappers)
  in
  let parse_ml = function
    | {pstr_desc=Pstr_extension (x, _)} -> Some x
    | _ -> None
  in
  let parse_mli = function
    | {psig_desc=Psig_extension (x, _)} -> Some x
    | _ -> None
  in
  {
    Ast_mapper.default_mapper
    with
      structure = apply_all parse_ml (fun m -> m.structure m);
      signature = apply_all parse_mli (fun m -> m.signature m);
  }

let () =
  Printexc.register_printer
    (function
      | Dynlink.Error err -> Some (Dynlink.error_message err)
      | _ -> None
    )

let () = Ast_mapper.run_main main

