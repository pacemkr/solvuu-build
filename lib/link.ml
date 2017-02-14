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


  type t = {
    dir: string;
    name: string;
    deps: string list;
    prods: string list;
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



  let create ?o_files ~ml_files ~dir ~name =
    let dir = Filename.normalize (dirname dir) in

    (* Link order matters for libraries.
     * `.cmo` files need to be listed in dependency order for ocamlmklib
     * or an invalid library will be produced. *)
    let cmo_files =
      Tools.run_ocamlfind_ocamldep_sort ml_files |>
      List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:".cmo") in

    let deps, prods = match o_files with
      | Some o_files ->
        let deps = o_files@cmo_files in
        let prods = [
          sprintf "%s/dll%s.so" dir name;
          sprintf "%s/lib%s.a" dir name;
          sprintf "%s/%s.cma" dir name;
        ] in
        (deps, prods)

      | None ->
        let prods = [
          sprintf "%s/%s.cma" dir name;
        ] in
        (cmo_files, prods)
    in

    {
      dir; name;
      deps; prods;
      ocamlmklib_opts = {
        pathL = [];
        l = [];
      }
    }


  let link_packages t ~packages =
    List.map ~f:Tools.run_ocamlfind_query packages |>
    List.fold_left ~f:add_ocamlmklib_pathL ~init:t


  (* Throw on no .o files *)
  let link_clibs t ~clibs =
    List.fold_left ~f:add_ocamlmklib_l ~init:t clibs


  let install_rules {dir; name; deps; prods; ocamlmklib_opts = {pathL; l} } =
    let o = sprintf "%s/%s" dir name in
    Rule.rule ~deps ~prods (fun _ _ ->
        Tools.ocamlmklib ~o ~pathL ~l deps
      )

end
