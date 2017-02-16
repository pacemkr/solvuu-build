open Printf
open Util
open Util.Filename


module Deps : sig
  type t
end =
struct
  type t = {
    dir : string;
    name : string;
    deps : string list;
  }


end

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
    sorted_deps : string list;
  }

  type t = lib_info * ocamlmklib_opts


  let stdlib_path = Tools.run_ocamlfind_query "stdlib"


  let create ~ml_files ~dir ~name = (
    let sorted_deps =
        Tools.run_ocamlfind_ocamldep_sort ml_files |>
        List.map ~f:(chop_suffix ".ml")
    in
    { dir = dirname dir; name;
      sorted_deps },
    {pathL = []; l = []}
  )

  let ocamldep_sort files =
    let ext = extension (List.hd files) in
    List.map ~f:(replace_suffix_exn ~old:ext ~new_:".ml") files |>
    Tools.run_ocamlfind_ocamldep_sort |>
    List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:ext)


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


  let output_of {dir; name; _} =
    sprintf "%s/%s" dir name


  let prods_of {dir; name; _} = function
    | `Clib -> [
        sprintf "%s/lib%s.a" dir name;
        sprintf "%s/dll%s.so" dir name;
      ]
    | `Bytecode -> [sprintf "%s/%s.cma" dir name ]
    | `Native ->  [sprintf "%s/%s.cmxa" dir name]
 (* | `Shared -> ... *)


  let deps_of libi = function
    | `Bytecode -> (prods_of libi `Clib)


  (* Build [.so] and [.a] files. *)
  (* TODO: Warn on missing flags that are likely to break the library. *)
  let install_stub_rules (libi, {pathL; l}) ~o_files =
    Rule.rule ~deps:o_files ~prods:(prods_of libi `Clib) (fun _ _ ->
        Tools.ocamlmklib ~o:(output_of libi) ~pathL ~l o_files
      )


  (* let install_lib_rules {pathL; l} ~ext files kind = *)
  (*   let files = ocamldep_sort files in *)
  (*   match kind *)
  (*   | `Byte -> *)



  let install_lib_rules (libi, {pathL; l}) ~deps = function
    | `Cma ->
      let deps = deps in
      Rule.rule ~deps ~prods:(prods info `Byte) (fun _ _ ->
          Tools.ocamlmklib ~verbose:() ~o:(output info) ~pathL ~l cmo_files
        )



  let install_bytecode_lib_rules2 = install_lib_rules `Cma


  (* Build [.cma] file. *)
  let install_bytecode_lib_rules (info, {pathL; l}) ~ml_files =
    let cmo_files =
      Tools.run_ocamlfind_ocamldep_sort ml_files |>
      List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:".cmo")
    in
    let deps = (prods info `Clib)@cmo_files in
    Rule.rule ~deps ~prods:(prods info `Byte) (fun _ _ ->
        Tools.ocamlmklib ~verbose:() ~o:(output info) ~pathL ~l cmo_files
      )


  (* Build .cmxa file. *)
  let install_native_lib_rules (info, {pathL; l}) ~ml_files =
    let cmx_files =
      Tools.run_ocamlfind_ocamldep_sort ml_files |>
      List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:".cmx")
    in
    let deps = (prods info `Clib)@cmx_files in
    Rule.rule ~deps ~prods:(prods info `Native) (fun _ _ ->
        Tools.ocamlmklib ~verbose:() ~o:(output info) ~pathL ~l cmx_files
      )
end
