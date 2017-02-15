open Printf
open Util
open Util.Filename

module Lib = struct

  (* TODO: May not be such a great idea to use a record here?
   *       Since this one structure will be used to generate multiple rules. *)

  type stubs = {
    o_files: string list;
    a_file: string;
    dll_file: string;
  }

  type files = {
    output: string;
    cmo_files: string list;
    cmx_files: string list;
    cma_file: string;
    cmxa_file: string;
    stubs: stubs option;
  }

  type ocamlmklib_opts = {
    pathL : string list;
    l : string list;
  }

  type t = files * ocamlmklib_opts


  let stdlib_path = Tools.run_ocamlfind_query "stdlib"


  let add_ocamlmklib_l (files, opts) clib =
    let l = List.cons_uniq clib opts.l in
    (files, {opts with l})


  let add_ocamlmklib_pathL ((files, opts) as t) path =
    if path = stdlib_path then t else
    (files, {opts with pathL = List.cons_uniq path opts.pathL})


  let create ?o_files ~ml_files ~dir ~name =
    let dir = Filename.normalize (dirname dir) in
    (* Link order matters for libraries.
     * `.cmo` files need to be listed in dependency order for ocamlmklib
     * or an invalid library will be produced. *)

    let (cmo_files, cmx_files) = (
      Tools.run_ocamlfind_ocamldep_sort ml_files |>
      List.map ~f:(fun mlf ->
        let rs = replace_suffix_exn ~old:".ml" mlf in
        (rs ~new_:".cmo", rs ~new_:".cmx")
      )) |>
      List.split
    in

    let stubs = match o_files with
      | Some o_files -> Some {
          o_files;
          a_file = sprintf "%s/lib%s.a" dir name;
          dll_file = sprintf "%s/dll%s.so" dir name;
        }
      | None -> None in

    let files = {
      cmo_files; cmx_files; stubs;
      cma_file = sprintf "%s/%s.cma" dir name;
      cmxa_file = sprintf "%s/%s.cmxa" dir name;
      output = sprintf "%s/%s" dir name;
    } in

    let opts = {
      pathL = [];
      l = [];
    } in

    (files, opts)



  let link_packages t ~packages =
    List.map ~f:Tools.run_ocamlfind_query packages |>
    List.fold_left ~f:add_ocamlmklib_pathL ~init:t


  (* Throw on no .o files *)
  let link_clibs t ~clibs =
    List.fold_left ~f:add_ocamlmklib_l ~init:t clibs


  (* TODO *)
  (* - Build .so/.a, .cma, .cmxa in three separate steps. *)
  (* - Pass cmx files. *)
  (* - For "native plugins" .cmxs we may need to build using ocamlc directly. *)
  (*   Or, install_rules_targeting_native_plugin which passes through extra arguments to ocamlopt. *1) *)
  let install_rules (
      {
        output = o;
        stubs;
        cmo_files;
        cmx_files;
        cma_file;
        cmxa_file;
      }, {
        pathL;
        l;
      }) =
    match stubs with
    | Some {o_files; a_file; dll_file} ->
      (* Build .so and .a files. *)
      let stub_libs = [a_file; dll_file] in
      Rule.rule ~deps:o_files ~prods:stub_libs (fun _ _ ->
          Tools.ocamlmklib ~o ~pathL ~l o_files
        );

      (* Build .cma file. *)
      Rule.rule ~deps:(stub_libs@cmo_files) ~prods:[cma_file] (fun _ _ ->
          Tools.ocamlmklib ~verbose:() ~o ~pathL ~l cmo_files
        );

      (* Build .cmxa file. *)
      Rule.rule ~deps:(stub_libs@cmx_files) ~prods:[cmxa_file] (fun _ _ ->
          Tools.ocamlmklib ~verbose:() ~o ~pathL ~l cmx_files
        );

    | None -> ()

end
