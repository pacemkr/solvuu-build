open Ocamlbuild_plugin
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
  val typ : t -> typ
  val path : t -> string

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

  let is_of_typ (typ2,_) ~typ = typ = typ2

  let typ (typ,_) = typ

  let path (_,path) = path

  let filter_by ~typ files =
    List.filter ~f:(is_of_typ ~typ) files

  let replace_extension_exn ((typ, path) : t) ~old ~new_ =
    (typ, replace_suffix_exn ~old:(typ_to_extension old) ~new_:(typ_to_extension new_) path)
end


module BTools = struct
  type t = spec option list list

  let ocamlc spec files =
    let open Util.Spec in
    [[Some (A "ocamlfind"); Some (A "ocamlc")]]
    @spec
    @[List.map files ~f:(fun file -> Some (A file))]
    |> specs_to_command
end


module Compile = struct
  type tool =
    | Ocamlc of BTools.t

  type t = {
    deps : File.t list;
    file : File.t;
    prods : File.t list;
    tool : tool;
  }

  (* TODO: Validate file type. *)
  let mli_file
      ?pathI
      ?package
      ?internal_deps:(internal_deps=[])
      ?thread
      file
    =
    let open File in
    let cmi_file = replace_extension_exn ~old:Mli ~new_:Cmi file in
    let spec = Util.Spec.([
        string_list ~delim:`Space "-package" package;
        string ~delim:`Space "-o" (Some (path cmi_file));
        unit "-thread" thread;
        string_list ~delim:`Space "-I" pathI;
      ])
    in
    {
     deps = file :: internal_deps;
     prods = [cmi_file];
     tool = Ocamlc spec;
     file;
    }


  let install_rules {deps; prods; tool; file} =
    let deps = List.map ~f:File.path deps in
    let prods = List.map ~f:File.path prods in
    match tool with
    | Ocamlc spec ->
      Rule.rule ~deps ~prods (fun _ _ ->
          BTools.ocamlc spec [File.path file]
        )




  (* Compile.mli_files ~deps:(files Mli) *)

  (*   (1* .mli -> .cmi *1) *)
  (*   List.iter mli_files ~f:(fun mli -> *)
  (*     let base = chop_suffix mli ".mli" in *)
  (*     let cmi = sprintf "%s.cmi" base in *)
  (*     Rule.rule ~deps:(mli::internal_deps) ~prods:[cmi] *)
  (*       (fun _ build -> *)
  (*          build_deps_cmi_files build ~pathI ~package file_base_of_module mli; *)
  (*          ocaml `Byte ~c:() ~pathI ~package ~o:cmi [mli] *)
  (*       ) *)
  (*   ); *)
end
