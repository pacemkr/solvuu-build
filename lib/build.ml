open Printf
open Util
open Util.Filename
(* open Build_tools *)

(* TODO: TESTS
 *
 * - Mli as first processed file with Ml dependencies.
 *   We're not using dynamic dependency capability of ocamlbuild since each run of SB
 *   puts together a tailored rule set just for that invocation. Hence, we can run any
 *   tools (just ocamldep in practice?) to get a list of dependencies at the time of invocation,
 *   and including any additional dependencies in ~deps.
 *
 *)




(* Core *)

type _ expr = ..


type 'hd exprs =
  | Exec : 'hd expr * 'tail exprs -> ('hd * 'tail) exprs
  | End : unit exprs

(* type cmds = Existential_cmds : 'cmds cmd -> cmds *)


type ('a, 'b, 'c) rule = {
  deps : 'a list;
  prods : 'b list;
  exprs : 'c exprs;
}


type _ artifact = ..







(* Ocaml *)

module Ob = Ocamlbuild_plugin

(* This mostly mints new types for each file type,
 * to prevent accidentally mixing up input formats for tools. *)
module File = struct
  module type Typ = sig
    type t
    val ext : string
    val of_path : string -> t
    val path : t -> string
    val replace_ext : ext:string -> t -> string
  end

  module Make(Ft : sig val ext : string end) : Typ = struct
    include Ft

    type t = string
    let of_path path = path
    let path t = t
    let replace_ext ~ext = replace_suffix_exn ~old:Ft.ext ~new_:ext
  end

  let typ_conv
    (type a)
    (type b)
    (module From : Typ with type t = a)
    (module To : Typ with type t = b)
    (file : a) : b
    =
    To.of_path (replace_suffix_exn ~old:From.ext ~new_:To.ext (From.path file))

  module Ml = Make(struct let ext = ".ml" end)
  module Mli = Make(struct let ext = ".mli" end)
  module Cmo = Make(struct let ext = ".cmo" end)
  module Cmi = Make(struct let ext = ".cmi" end)
  module Cmx = Make(struct let ext = ".cmx" end)
  module Cma = Make(struct let ext = ".cma" end)
  module Cmxa = Make(struct let ext = ".cmxa" end)
  module Cmxs = Make(struct let ext = ".cmxs" end)
  module C = Make(struct let ext = ".c" end)
  module O = Make(struct let ext = ".o" end)
  module A = Make(struct let ext = ".a" end)
  module Dll = Make(struct let ext = ".so" end)
end

open File


(* type 'hd expr_list = *)
(*   | Lis : 'hd expr * 'next expr_list -> ('hd * 'next) expr_list *)
(*   | End : unit expr_list *)

(* module Tool = struct *)
(*   module type S = sig *)
(*   end *)

  (* type ocamlc_compile_flags = .. *)
  (* type ocamlc_compile_flags += *)

  (* type t = .. *)
  (* type t += *)
  (*   | Compile of ocamlc_compile_flags list *)
(*| Archive of File.Mli.t ocamlc_output *)

  (* let flags_to_spec = List.map ~f:(function *)
  (*     | O cmi_file -> (Ob.A (Cmi.path cmi_file)) *)
  (*     | _ -> raise Not_found *)
  (*   ) *)

  (* let to_spec = function *)
  (*   | Compile flags -> (Ob.A "-c") :: (flags_to_spec flags) *)
  (*   | _ -> raise Not_found *)

module Tool = struct
  (* type 'a flag *)

  (* type _ expr += *)
  (*   | Flg : 'hd flag expr * 'tail flag expr -> ('hd * 'tail) flag expr *)
  (*   | End : unit flag expr *)
end

module Ocamlc = struct
  (* | O : File.Cmi.t * 'a expr_list -> File.Cmi.t expr *)
  type 'a t
  (* type ('a, 'b) tt = ('a t * 'b t) expr *)

  (* type ('typ, 'tail_hd, 'tail_tail, 'tail) flag = *)
  (*   'typ * ('tail_hd t * 'tail_tail t) expr -> ('typ t * 'tail t) expr *)


  type ('typ, 'a) flag = ('typ t * 'a t)
(* string * ('a t * 'b t) expr -> (string t * 'c t) *)

  type _ expr +=
    (* | Flags : 'a * 'b expr -> ('b * t) expr *)
    (* | O : string * ('a expr) -> (string * ('a expr)) expr *)
    (* | I : unit list * ('a expr) -> (unit list * ('a expr)) expr *)
    | O : string * ('a, 'b) flag expr -> (string, 'c) flag expr
    | I : string * ('a, 'b) flag expr -> (string, 'c) flag expr
    (* | I : string * ('a t * 'b t) expr -> (string t * 'c t) expr *)
    (* | I : string * ('a t) expr -> (string * ('a t) expr) t expr *)
    (* | O : string * ('a * 't) expr -> (string * ('a t expr)) expr *)
    | End : (unit t * unit t) expr
end

module Ocamlopt = struct
(* (1*   include Tool *1) *)

(*   type 'a flag *)
(*   type t *)

(*   let flg : 'a -> 'a flag = fun a -> a *)

  type 'a t
  type _ expr +=
    | I : string * ('a t * 'b t) expr -> (string t * ('a t)) expr

(*   type _ expr += *)
(*     | O : string flag * ('a expr) -> (string flag * ('a expr)) expr *)
(*     (1* | End : t expr *1) *)

(* (1*     | O : File.Mli.t -> File.Mli.t flag expr *1) *)
(* (1*     | I : string -> string expr *1) *)
(* (1*     | V : unit expr *1) *)
end


let make_expr =
  (* Exec (Ocamlc.(O ("file.out", End)), End) *)
  Exec (
    Ocamlc.O ("file.out",
              Ocamlc.I ("p/a/t/h",
                        Ocamlopt.I ("p/a/t/h",
                                    Ocamlc.End))),
    End)
           (* Run (Ocamlopt.(Flg (V, End)), *)
           (*      End)) *)


(* type _ expr += *)
  (* | Ocamlc : 'a Ocamlc.flags -> 'a Ocamlc.flags expr *)


(* type _ artifact += *)
(*   | Compiled_interface : (Mli.t, Cmi.t, 'a) rule -> (File.Mli.t, File.Cmi.t, 'a) rule artifact *)


(* let cmd_to_spec : type a . a cmd -> Ob.spec list = function *)
(*   | Ocamlc t -> (Ob.A "ocamlc") :: (Ocamlc.to_spec t) *)
(*   | _ -> raise Not_found *)


(* let rec cmds_to_spec : type a . a cmds -> Ob.Command.t list = function *)
(*   | Run (cmd, next) -> (Ob.Cmd (Ob.S (cmd_to_spec cmd))) ::  (cmds_to_spec next) *)
(*   | End -> [] *)


(* let compile_mli mli_file = *)
(*   let open File in *)
(*   let cmi_file = typ_conv (module Mli) (module Cmi) mli_file in *)
(*   Compiled_interface { *)
(*     deps = [mli_file]; *)
(*     prods = [cmi_file]; *)
(*     (1* exprs = (Run (Ocamlc Ocamlc.(Compile ((O cmi_file) :: ocamlc_flags)), End)); *1) *)
(*     exprs = ( *)
(*       Run (Ocamlc.(Flg (Ocamlopt.V, End)), End) *)
(*            (1* Run (Ocamlopt.(Flg (V, End)), *1) *)
(*            (1*      End)) *1) *)
(*     ); *)
(*   } *)



(* let to_cmd extend lookup expr = function *)
(*   | unknown_expr -> lookup unknown_expr *)




(* module Custom_tools = Ocaml_tools.Make(struct *)
(*     module Ocamlc = Ocamlc.Make( *)
(*   end) *)


(* module Build_ocamlfuse = Build.Make(struct *)
(*   module Ocamlc = struct *)
(*     include Ocaml_tools.Ocamlc *)

(*     type flags += V *)

(*     let to_spec = function *)
(*       | V -> *)
(*       | a -> Ocamlc.to_spec(a) *)
(*   end *)
(* end) *)


(* type ocamlc_flag = .. *)
(* type ocamlc_flag += *)
(*   | Mlc_o of string *)

(* type ocamlc_expr = ocamlc_flag list *)

(* type _ expr = .. *)
(* type _ expr = *)
(*   (1* | Unit : spec list expr *1) *)
(*   (1* | String : (string * (string -> spec list)) expr *1) *)
(*   (1* | String_list : (string list * (string list -> spec list)) expr *1) *)
(*   (1* | Set_o : string -> 'a list expr *1) *)
(*   (1* | Set_c : string -> 'b list expr *1) *)
(*   | Ocamlc : ocamlc_expr -> ocamlc_expr expr *)


(* let rec expr_to_cmd : type a b . a expr -> b expr = *)
(*   match expr with *)
(*   | Ocamlc flags -> expr_to_cmd flags *)
  (* | Set_o string -> *)
  (* | other -> expr_to_cmd (extend other) *)

(* type cmd = .. *)
(* type cmd += *)
(*   | Ocamlc of Ocamlc.t *)
(*   | Ocamlfind_ocamlc of Ocamlfind.Ocamlc.t *)


(* type tool = cmd ToolList.t *)

(*
type rule = {
  deps : string list;
  prods : string list;
  (* spec : expr; *)
}


type artifact = ..
type artifact +=
  | Intf of File.Mli.t
  | Compiled_intf of rule * File.Cmi.t
  | Rule of rule


let is_rule = function Rule _ -> true | _ -> false


let to_command spec files =
  let open Util.Spec in
  [[Some (A "ocamlfind"); Some (A "ocamlc")]]
  @[[Some (A "-verbose")]]
  @spec
  @[List.map files ~f:(fun file -> Some (A file))]
  |> specs_to_command


let ls_dir dir =
  let open File in
  let all_files =
    try Sys.readdir dir |> Array.to_list
    with _ -> []
  in
  List.filter_map all_files ~f:(fun path ->
      match extension path with
      | ".mli" -> Some (Intf (Mli.of_path (dir ^ "/" ^ path)))
      (* | ".ml" -> Some (Src (Ml.of_path (dir ^ "/" ^ path))) *)
      | _ -> None
    )


let compile_mli ~ocamlc mli_file =
  let open File in
  let cmi_file = typ_conv (module Mli) (module Cmi) mli_file in
  let cmi_path = Cmi.path cmi_file in
  let mli_path = Mli.path mli_file in
  let cmds = [
    (Ocamlcc [(Mlc_o cmi_path)])

    (* Ocamlfind_ocamlc Ocamlfind.Ocamlc.( *)
    (*     set_o ocamlc ~v:cmi_path *)
    (*   ); *)
  ] in
  Compiled_intf ({
      deps = [mli_path];
      prods = [cmi_path];
      spec;
    }, cmi_file)


(* TODO: Check for infinite recursion. Prods = deps will recurse infinitely. *)
(*       Use a graph to keep track of circular dependecies at each recursion. *)
let rec build
    ?(is_target=(List.for_all ~f:is_rule))
    ?(init=[])
    ~f
    deps
  =
  if (is_target deps) then deps
  else build ~is_target ~f (List.fold_left ~init ~f deps)


let install_rules rules =
  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.After_rules -> (

      Ocamlbuild_plugin.clear_rules();

      List.filter_map ~f:(function Rule r -> Some r | _ -> None) rules |>
      List.iter ~f:(fun {deps; prods; cmds} ->
          Rule.rule ~deps ~prods (fun _ _ ->


              (* get tool from cmd variant *)


              List.map cmds ~f:(function
                  | (t : cmd) -> Tool.to_spec t

                  (* | a -> a *)
                )
            )
        )
    )
  | _ -> ()


let build_lib ~dir ~findlib_deps artifact =
  let open Ocamlfind in
  let create (module Common : Ocamlx) =
    Common.(
      create () |>
      set_verbose |>
      set_package ~v:findlib_deps |>
      set_pathI ~v:[dir]
    )
  in
  let ocamlc = create (module Ocamlc) in
  (* let ocamlopt = create (module Ocamlopt) in *)
  match artifact with
  (* | `Src _ -> dep *)
  | Intf mli_file -> (compile_mli ~ocamlc mli_file) (* <- list of prods *)
  | Compiled_intf (rule, _) -> (Rule rule)
  (* | Rule {deps; prods; cmds} -> *)
  (*   Rule.rule ~deps ~prods (fun _ _ -> *)
  (*       List.map cmds ~f:(function *)
  (*           | tool : Tool as t -> Tool.to_spec t *)
  (*           | a -> a *)
  (*         ) *)
  (*     ) *)
  | a -> a


let lib prods dep =
  (
    build_lib dep |>
    pathI_for_cmi
  )
  :: prods


let b =

    ls_dir "lib" |>
    (* build ~f:lib |> *)
    build ~f:(fun prods dep ->

        (
          build_lib dep |>
          function
          | (_,
          Build.eval (Ocamlc Set_verbose)
          (* (function *)
          (*   | Compiled_intf (_, rule) -> *)
          (*     set_flag Ocamlc "-thread" *)
          (* ) *)
        )
        :: prods

      ) |>
    (* flag Ocamlc "-thread" |> *)
    install_rules




(* let re build ?next ~step ~deps = *)
(*   let prods = List.fold_left ~f:step ~init:[] deps in *)
(*   let prods = match next with *)
(*   | Some next -> (List.fold_left ~f:next ~init:[] prods) @ prods *)
(*   | None -> prods *)
(*   in *)
(*   if list.for_all ~f:is_rule prods then prods *)
(*   else build ?next ~step ~deps:prods *)

(* let (|>>) deps next_build_step = *)



(* let lib ~dir = *)
(*   Ocamlbuild_plugin.dispatch @@ function *)
(*   | Ocamlbuild_plugin.After_rules -> ( *)
(*       Ocamlbuild_plugin.clear_rules(); *)

(*       ls_dir dir |> *)

(*       build ~f:(fun prods dep -> *)
(*           ( *)
(*             build_lib dep |> *)
(*             pathI_for_cmi *)
(*           ) *)
(*           :: prods *)
(*         ) *)
(*       |> install_rules *)

(*     ) *)
(*   | _ -> () *)


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
(*
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
   *)
