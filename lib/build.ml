open Printf
open Util
open Util.Filename

module File : sig
  type t

  type typ =
    | Ml (* source code! *)
    | Mli (* interface! *)
    | Cmo (* bytecode object *)
    | Cmi (* compiled interface, identical for bytecode and native object *)
    | Cmx (* native cross-module optimization (inlining) file *)
    | Cma (* bytecode lib *)
    | Cmxa (* native lib *)
    | Cmxs (* shared lib *)
    | C (* C source code *)
    | C_o (* C obj *)
    | C_a (* C static lib *)
    | C_dll (* C dynamic lib *)

  val create : typ -> string -> t
  val typ_to_extension : typ -> string
  val is_of_typ : t -> typ:typ -> bool
  val replace_extension_exn : t -> old:typ -> new_:typ -> t
  val filter_by : typ:typ -> t list -> t list

end = struct

  type typ =
    | Ml (* source code! *)
    | Mli (* interface! *)
    | Cmo (* bytecode object *)
    | Cmi (* compiled interface, identical for bytecode and native object *)
    | Cmx (* native cross-module optimization (inlining) file *)
    | Cma (* bytecode lib *)
    | Cmxa (* native lib *)
    | Cmxs (* shared lib *)
    | C (* C source code *)
    | C_o (* C obj *)
    | C_a (* C static lib *)
    | C_dll (* C dynamic lib *)

  type t = typ * string

  let typ_to_extension = function
    | Ml -> ".ml"
    | Mli -> ".mli"
    | Cmo -> ".cmo"
    | Cmi -> ".cmi"
    | Cmx -> ".cmx"
    | Cma -> ".cma"
    | Cmxa -> ".cmxa"
    | Cmxs -> ".cmxs"
    | C -> ".c"
    | C_o -> ".o"
    | C_a -> ".a"
    | C_dll -> ".so"


  let create typ path =
    (typ, path)

  let is_of_typ (typ2, _) ~typ = typ = typ2

  let filter_by ~typ files =
    List.filter ~f:(is_of_typ ~typ) files

  let replace_extension_exn ((typ, path) : t) ~old ~new_ =
    (typ, replace_suffix_exn ~old:(typ_to_extension old) ~new_:(typ_to_extension new_) path)
end




module Compile = struct
  type opts = {
    a : string;
  }

  type tool =
    | Ocamlc of opts
    | Ocamlopt of opts

  type t = {
    deps : File.t list;
    files : File.t list;
    prods : File.t list;
    tool : tool;
  }


  let mli_files ~deps ~files =
    let open File in
    let files = filter_by ~typ:Mli files in
    let prods = List.map ~f:(replace_extension_exn ~old:Mli ~new_:Cmi) files in
    {
     deps;
     files;
     prods;
     tool = Ocamlc {
       a = "";
     };
    }


  (* Compile.mli_files ~deps:(files Mli) *)

  (*   (1* .mli -> .cmi *1) *)
  (*   List.iter mli_files ~f:(fun mli -> *)
  (*     let base = chop_suffix mli ".mli" in *)
  (*     let cmi = sprintf "%s.cmi" base in *)
  (*     let internal_deps = internal_deps_files `Byte (Lib x) in *)
  (*     Rule.rule ~deps:(mli::internal_deps) ~prods:[cmi] *)
  (*       (fun _ build -> *)
  (*          build_deps_cmi_files build ~pathI ~package file_base_of_module mli; *)
  (*          ocaml `Byte ~c:() ~pathI ~package ~o:cmi [mli] *)
  (*       ) *)
  (*   ); *)

end
