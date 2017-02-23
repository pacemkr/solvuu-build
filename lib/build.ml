open Ocamlbuild_plugin
open Printf
open Util
open Util.Filename


module File = struct
  module type Typ = sig
    type t
    val of_path : string -> t
    val path : t -> string
  end

  module Simple_file = struct
    type t = string
    let of_path path = path
    let path t = t
  end
    (* let of_path path = *)
    (*   match extension path with *)
    (*   | ".mli" -> Some (`Mli path) *)
    (*   | _ -> None *)

  let path
    (type a)
    (module F : Typ with type t = a)
    (t : a)
    =
    F.path t

  module Mli : Typ = Simple_file
  module Ml : Typ = Simple_file
  (* let path t : Typ = *)
  (*   let module M = (val m : Typ with type t = string) in *)
  (*   M.path t *)
end



(* let () = *)
(*   let open File in *)
(*   let testf = Mli.of_path "asd" in *)
(*   let mli_typ = (module Mli : Typ with type t = Mli.t) in *)
(*   let ml_typ = (module Ml : Typ with type t = Ml.t) in *)
(*   let (module M) = mli_typ in *)
(*   let (module M) = ml_typ in *)
(*   let path = M.path testf in *)
(*   () *)

  (* module Make(Typ : sig val ext : string end) : sig *)
  (*   type t *)
  (*   val of_path : string -> t *)
  (*   val path : t -> string *)
  (* end = struct *)
  (*   type t = string *)
  (*   (1* TODO: validate extension *1) *)
  (*   let of_path path = path *)
  (*   let path t = t *)
  (* end *)

  (* type ml = string *)
  (* type mli *)

  (* type 'a file = 'a * string *)

  (* let create typ path = (typ, path) *)
  (* module Ml = Make(struct let ext = ".ml" end) *)
  (* module Mli = Make(struct let ext = ".mli" end) *)

  (* let path (_, path) = path *)


let ls_dir dir =
  let open File in
  let all_files =
    try Sys.readdir dir |> Array.to_list
    with _ -> []
  in
  List.filter_map all_files ~f:(fun path ->
      match extension path with
      | ".mli" -> Some (`Mli (Mli.of_path path))
      | ".ml" -> Some (`Ml (Ml.of_path path))
      | _ -> None
    )



  (* let ml_files = select_files ".ml" in *)
  (* let mli_files = select_files ".mli" in *)
  (* let c_files = select_files ".c" in *)
  (* List.map ~f:(fun f -> `Mli (Mli.of_path f)) mli_files *)
(* List.map ~f:(create Ml) ml_files @ *)
(* List.map ~f:(create C) c_files *)


let () =
  let open File in
  ls_dir "." |>
  List.iter ~f:(function
      | `Mli file -> print_endline (File.path (module Mli) file);
      | `Ml file -> print_endline (File.path (module Ml) file);
    )








(*
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
  val ls_dir : dir:string -> t list

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

  let ls_dir ~dir =
    let all_files =
      try Sys.readdir dir |> Array.to_list
      with _ -> []
    in
    let select_files ?add_replace suffix =
      List.filter all_files ~f:(fun x -> check_suffix x suffix) |> fun l ->
      (match add_replace with
       | None -> l
       | Some (`Add x) -> x@l
       | Some (`Replace x) -> x
      ) |>
      List.sort_uniq ~cmp:String.compare
    in
    let ml_files = select_files ".ml" in
    let mli_files = select_files ".mli" in
    let c_files = select_files ".c" in
    List.map ~f:(create Mli) mli_files
    (* List.map ~f:(create Ml) ml_files @ *)
    (* List.map ~f:(create C) c_files *)

  let is_of_typ (typ2,_) ~typ = typ = typ2

  let typ (typ,_) = typ

  let path (_,path) = path

  let filter_by ~typ files =
    List.filter ~f:(is_of_typ ~typ) files

  let replace_extension_exn ((typ, path) : t) ~old ~new_ =
    (typ, replace_suffix_exn ~old:(typ_to_extension old) ~new_:(typ_to_extension new_) path)

(* (1* Sort files in dependency order before performing any actions. *)
(*  * Including prioritorizing mli files over ml, etc.*)
(* let sort_files () = () *)
end

(* Compile module takes prods to deps.
 * Thats a generalization on the entire Build process... *)
(* Thats what ocamlbuild does itself
 *
 * So....
 *
 * Add ability to specify extra constraints on top of ocamlbuild commands.
 *  - What dependencies are required. -> ocamlbuild does this with ~deps
 *  - What combination of flags is required
 *  - What kind of dependency to product mapping is possible in the first place,
 *    ie. what are the toolchain capabilities. This is effectively limiting what
 *    combination of deps and prods you can have. Is this useful?
 *  - Perhaps more useful: I want -this type of output-
 *                         From -these files-
 *                         What are the steps and opts required to the toolchain.
 *  - This is effectively what you are describing with Ocaml build, hence the tags thing probably.
 *
 *
 * *)
module Ocamlc = struct
  type t = {
    deps : File.t list;
    prods : File.t list;
    files : File.t list;
    spec : spec option list list;
  }

  let package t package =
    let spec = Util.Spec.(
        (string_list ~delim:`Space "-package" package) :: t.spec
      ) in
    {t with spec}


  let pathI t pathI =
    let spec = Util.Spec.(
        (string_list ~delim:`Space "-I" pathI) :: t.spec
      ) in
    {t with spec}


  let thread t =
    let spec = Util.Spec.(
        (unit "-thread" (Some ())) :: t.spec
      ) in
    {t with spec}


  let compile_mli ~opts file =
    let open File in
    let cmi_file = replace_extension_exn ~old:Mli ~new_:Cmi file in
    let spec = Util.Spec.(opts @ [
        string ~delim:`Space "-o" (Some (path cmi_file));
      ])
    in
    {
      deps = [file];
      prods = [cmi_file];
      spec;
      files = [file];
    }


  let to_command t =
    let open Util.Spec in
    [[Some (A "ocamlfind"); Some (A "ocamlc")]]
    @t.spec
    @[List.map t.files ~f:(fun file -> Some (A (File.path file)))]
    |> specs_to_command


  (* let ml_file *)
  (*     ?internal_deps:(internal_deps=[]) *)
  (*     files *)
  (*     kind *)
  (*   = *)
  (*   let open File in *)
  (*     let cmo_file = replace_extension_exn ~old:Ml ~new_:Cmo in *)


  (* Take common opts for tools. Internal deps, pathI, etc. *)
  (* let create ~file = *)
  (*   match File.typ file with *)
  (*   | Mli -> mli_file file *)
  (*   | _ -> raise Unsupported_file_type file *)


  let install_rules ({deps; prods;_} as t) =
    let paths = List.map ~f:File.path in
    let deps = paths deps in
    let prods = paths prods in
    Rule.rule ~deps ~prods (fun _ _ ->
        to_command t
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


(* Mint new type for each file type to prevent problems *)

(* Can use one or a combination of tools *)
(* Pass file to Tool for command generation. *)
(* Customize args at each step, by using opts with the same file glob as ocamlbuild tags? *)
(* Install rules. *)
module BuildLib = struct
  type t = {
    name : string;
    dir : string;
    packages : string list option;
  }

  (* Make it impossible to call functions with wrong input file type, or wrong dependency *)
  type 'a file = 'a * string


  let pathI t =
    Util.Spec.string_list ~delim:`Space "-I" (Some [
        t.dir;
        (* internal deps paths *)
      ])


  let package t =
    Util.Spec.string_list ~delim:`Space "-package" t.packages


  let compile_mli t mli_file = `Compiled_intf (
      Ocamlc.compile_mli ~opts:[
        (package t);
        (pathI t);
      ] mli_file
    )


  let deps_of_files =
    List.map ~f:(fun file ->
        match File.typ file with
        | File.Mli -> `Intf file
        (* | File.C -> `C file *)
        | _ -> raise Not_found
      )


  (* This dispatch table is the plugin... *)
  (* You start here, and start adding support for more file types,
   * as the compiler reports errors... *)
  (* Tools should return prods in order that they should be build
   * that the consumer MUST handle or incomplete match. *)
  (* Calling step recurses into the prods right now,
   * not calling it finishes processing the current list of files first. *)
  (* Fold over dependency sorted files? *)

  let build_static_file path content =
    let open Ocamlbuild_plugin in
    let open Util in
    let path = Filename.normalize path in
    let content = List.map content ~f:(sprintf "%s\n") in
    rule path ~prod:path (fun _ _ ->
        Seq [
          Cmd (Sh (sprintf "mkdir -p %s" (dirname path)));
          Echo (content,path);
        ]
      )

  let makefile t =
    let default = ["default: byte"] in
    let ln file = [
      sprintf "%s: _build/%s" file file;
      "\tln -fs $< $@";
    ]
    in
    let libs = ["_build"/t.dir/t.name ^ ".cma"] in
    let byte =
      libs |>
      String.concat ~sep:" " |>
      sprintf "byte: %s"
    in
    (* let native = *)
    (*   [libs `Native;] |> *)
    (*   List.concat |> *)
    (*   String.concat ~sep:" " |> *)
    (*   sprintf "native: %s" *)
    (* in *)
    let outsource_to_ocamlbuild = [
      "_build/%: FORCE";
      "\t$(OCAMLBUILD) $(patsubst _build/%,%,$@)";
      "\trm -f $(notdir $@)";
    ]
    in
    let clean = [
      "clean:";
      "\trm -rf _build";
      sprintf "\trm -f .merlin .ocamlinit %s.install" t.name;
    ]
    in
    let phony = [".PHONY: default byte native clean"] in
    [
      default;
      outsource_to_ocamlbuild;
      ln @@ sprintf "%s.install" t.name;
      [byte];
      clean;
      phony;
      ["FORCE:"];
    ] |>
    List.intersperse ~sep:[""] |>
    List.flatten


  (* build is a list of customizers *)
  let rec build t (*?customizer*) deps =
    let prods = List.fold_left ~f:(
        fun prods -> function
          | `Intf mli_file -> (compile_mli t mli_file) :: prods (* <- list of prods *)
          | `Compiled_intf rules -> `Prod (dep, rules)
      ) ~init:[] deps
    in
    customizer prods
    if all `Prods then prods
    else build prods

      Build.ls_dir |>>
      Build.lib |>>
      Build.install_rules

  let rec chain build_step deps =
    let prods = List.fold_left ~f:build_step deps in


  (* Can you infer which tool to execute?
   * No, there are multiple ways to build a cmo. *)
  let lib ?findlib_deps ~dir ~name =
    Ocamlbuild_plugin.dispatch @@ function
    | Ocamlbuild_plugin.After_rules -> (
        Ocamlbuild_plugin.clear_rules();

        let t = {dir; name; packages = findlib_deps} in

        build_static_file "project.mk" (makefile t);

        File.ls_dir ~dir |>
        deps_of_files |>
        (* ocamldep_sort |> (1* Can be file sort *1) *)
        build t
      )
    | _ -> ()



    (* let compile = List.map mli_files ~f:(fun file -> Compile.mli_file *)
    (*                          ?thread *)
    (*                          ~package *)
    (*                          ~pathI *)
    (*                          ~internal_deps:cmi_internal_deps *)
    (*                          (File.create File.Mli file) *)
    (*                       ) in *)

    (* List.iter ~f:Compile.install_rules built_cmi; *)



    (* For each input file execute some command. *)
    (* Or, for a list of input files, execute commands. *)

    (* Work backwards through unit deps *)
    (* Lib < cma *)
    (*     < [name].a/.so *)
    (*     < cmo list *)
    (*     < ml *)
    (*     < mli *)


    (* build mli files *)
    (* build ml files *)
end
   *)


(* Ocamlc.with_new_opt : experimental_opts -> M : Ocamlc *)

(* module CompileBytecode = BuildWith(Ocamlc) *)
