open Util
open Util.Filename

module Lib = struct

  type t = {
    o_files : string list option;
    cmo_files : string list;
  }

  let create ?o_files ~package ~ml_files  =
    let sorted_deps = Tools.run_ocamlfind_ocamldep_sort ~package ml_files in
    let cmo_files = List.map ~f:(replace_suffix_exn ~old:".ml" ~new_:".cmo") sorted_deps in
    {o_files; cmo_files}

(*
            link_pkg ~pkg:"unix" |>
            link_pkg ~pkg:"threads" |>
            link_pkg ~pkg:"bigarray" |>
            link_pkg ~pkg:"camlidl" |>
*)

end
