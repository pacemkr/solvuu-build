open Util
open Util.Filename

module Lib = struct

  type ocamlmklib_opts = {
    pathL : string list;
    l : string list;
  }


  type t = {
    o_files : string list option;
    cmo_files : string list;
    ocamlmklib_opts : ocamlmklib_opts;
  }



  let stdlib_path = Tools.run_ocamlfind_query "stdlib"


  let add_ocamlmklib_l t clib =
    let l = List.cons_uniq clib t.ocamlmklib_opts.l in
    {t with ocamlmklib_opts = {t.ocamlmklib_opts with l}}


  let add_ocamlmklib_pathL t path =
    if path = stdlib_path then t else
    let pathL = List.cons_uniq path t.ocamlmklib_opts.pathL in
    {t with ocamlmklib_opts = {t.ocamlmklib_opts with pathL}}



  let create ~o_files ~ml_files =
    (* Link order matters for libraries.
     * `.cmo` files need to be listed in dependency order for ocamlmklib
     * or an invalid library will be produced. *)
    let cmo_files =
      Tools.run_ocamlfind_ocamldep_sort ml_files |>
      List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:".cmo") in

    {o_files; cmo_files;
     ocamlmklib_opts = {
       pathL = [];
       l = [];
     }
    }


  let link_packages t ~packages =
    List.map ~f:Tools.run_ocamlfind_query packages |>
    List.fold_left ~f:add_ocamlmklib_pathL ~init:t


  let link_clibs t ~clibs =
    List.fold_left ~f:add_ocamlmklib_l ~init:t clibs


end
