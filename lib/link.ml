open Util
open Util.Filename

module Lib = struct

  type ocamlmklib_opts = {
    pathL : string list;
  }


  type t = {
    o_files : string list option;
    cmo_files : string list;
    ocamlmklib_opts : ocamlmklib_opts;
  }


  let stdlib_path = Tools.run_ocamlfind_query "stdlib"


  let add_ocamlmklib_pathL t path =
    if path = stdlib_path then t else
    let pathL = t.ocamlmklib_opts.pathL in
    let pathL = if List.mem path ~set:pathL then
        pathL else path :: pathL
    in
    {t with ocamlmklib_opts = {
         t.ocamlmklib_opts with pathL
       }}


  let link_pkg t package =
    add_ocamlmklib_pathL t (Tools.run_ocamlfind_query package)


  let create ~package ~o_files ~ml_files =
    (* Link order matters for libraries.
     * `.cmo` files need to be listed in dependency order for ocamlmklib,
     * or an invalid library will be produced. *)
    let cmo_files =
      Tools.run_ocamlfind_ocamldep_sort ~package ml_files |>
      List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:".cmo") in

    let ocamlmklib_opts = {pathL = []} in
    let t = {o_files; cmo_files; ocamlmklib_opts} in
    (* By default link the packages *)
    List.fold_left ~f:link_pkg ~init:t package

end
