open Printf
open Util
open Util.Filename

module Lib = struct
  (* TODO: May not be such a great idea to use a record here?
   *       Since this one structure will be used to generate multiple rules. *)

  type ocamlmklib_opts = {
    pathL : string list;
    l : string list;
  }

  type lib_info = {
    dir : string;
    name : string;
  }

  type t = lib_info * ocamlmklib_opts


  let stdlib_path = Tools.run_ocamlfind_query "stdlib"


  let create ~dir ~name = (
    {dir = dirname dir; name},
    {pathL = []; l = []}
  )


  let add_ocamlmklib_l (files, opts) clib =
    let l = List.cons_uniq clib opts.l in
    (files, {opts with l})


  let add_ocamlmklib_pathL ((files, opts) as t) path =
    if path = stdlib_path then t else
    (files, {opts with pathL = List.cons_uniq path opts.pathL})


  (* These -L opts are needed to statically link with stubs of other packages. *)
  let link_packages t ~packages =
    List.map ~f:Tools.run_ocamlfind_query packages |>
    List.fold_left ~f:add_ocamlmklib_pathL ~init:t


  let link_clibs t ~clibs =
    List.fold_left ~f:add_ocamlmklib_l ~init:t clibs


  let output {dir; name} =
    sprintf "%s/%s" dir name


  let stub_prods {dir; name} = [
    sprintf "%s/lib%s.a" dir name;
    sprintf "%s/dll%s.so" dir name;
  ]


  let bytecode_prods {dir; name} = [
    sprintf "%s/%s.cma" dir name
  ]

  let native_prods {dir; name} = [
    sprintf "%s/%s.cmxa" dir name
  ]


  (* Build [.so] and [.a] files. *)
  (* TODO: Warn on missing flags that are likely to break the library. *)
  let install_stub_rules (info, {pathL; l}) ~o_files =
    Rule.rule ~deps:o_files ~prods:(stub_prods info) (fun _ _ ->
        Tools.ocamlmklib ~o:(output info) ~pathL ~l o_files
      )

  (* Build [.cma] file. *)
  let install_bytecode_lib_rules (info, {pathL; l}) ~ml_files =
    let cmo_files =
      Tools.run_ocamlfind_ocamldep_sort ml_files |>
      List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:".cmo")
    in
    let deps = (stub_prods info)@cmo_files in
    Rule.rule ~deps ~prods:(bytecode_prods info) (fun _ _ ->
        Tools.ocamlmklib ~verbose:() ~o:(output info) ~pathL ~l cmo_files
      )


  (* Build .cmxa file. *)
  let install_native_lib_rules (info, {pathL; l}) ~ml_files =
    let cmx_files =
      Tools.run_ocamlfind_ocamldep_sort ml_files |>
      List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:".cmx")
    in
    let deps = (stub_prods info)@cmx_files in
    Rule.rule ~deps ~prods:(native_prods info) (fun _ _ ->
        Tools.ocamlmklib ~verbose:() ~o:(output info) ~pathL ~l cmx_files
      )
end
