open Printf
open Util
open Util.Filename

(* TODO: TESTS
 *
 * - Mli as first processed file with Ml dependencies.
 *   We're not using dynamic dependency capability of ocamlbuild since each run of SB
 *   puts together a tailored rule set just for that invocation. Hence, we can run any
 *   tools (just ocamldep in practice?) to get a list of dependencies at the time of invocation,
 *   and including any additional dependencies in ~deps.
 *
 *)

(* Minimal tools and example to make creation of domain specific languages simpler.
 * Expressions and their relationships are defined by extending a single GADT type.
 **)
module Dsl = struct
  type exn += Fwhale of string

  (* Extensible expression vocabulary. *)
  type _ expr = ..

  type eexpr = Expr : 'a expr -> eexpr


  (* Non-unifying type minter. *)
  module Typ () = struct
    type 'a a
    type 'a t = 'a a
  end

  (* XXX Traps may need to take eexpr, but nothing else? *)

  module Kern = struct
    type ('a, 'b) trap = (eexpr -> 'a) * eexpr * 'b
  end
end


module Build = struct
  include Dsl
end


module Build_ocaml = struct
  include Build

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

    module T = Dsl.Typ ()
    type _ expr +=
      | Cmi : Cmi.t * 'a T.t expr -> eexpr T.t expr
      | Mli : Mli.t * 'a T.t expr -> eexpr T.t expr
      | End : eexpr T.t expr

    let rec to_string = function Expr expr -> (match expr with
        | Cmi (f, fs) -> (Cmi.path f) :: to_string (Expr fs)
        | Mli (f, fs) -> (Mli.path f) :: to_string (Expr fs)
        | End -> []
        | _ -> raise (Fwhale "Uknonwn file type.")
      )
  end

  module Proc (Ret : sig type t end) = struct
    type t

    type _ expr +=
      | Ret : t expr
      | Trap : (eexpr -> Ret.t) * eexpr * t expr -> t expr
      | Ptrap : Ret.t * t expr -> t expr


    let trap f = function
      | Trap (trap, expr, exprs) -> f (Ptrap (trap expr, exprs))
      | a -> f a

  end


  (* module Expr = struct *)
  (*   module T = Typ2 () *)

  (*   (1* type 'b teexpr = Tx : 'a expr -> 'b teexpr *1) *)

  (*   type ('a, 'b) x = 'a * 'b expr *)

  (* end *)

  type _ eexpr2 = Eexpr : 'b expr -> 'a eexpr2


  (* type _ expr2 = X : 'a expr -> 'a eexpr2 expr2 *)

  (* type _ expr += *)
  (*   (1* | X : 'a expr -> 'a eexpr2 expr *1) *)
  (*   | Ret : eexpr expr *)

  module Ocamlc = struct
    include (Proc (struct type t = Ob.spec list end))

    type _ expr +=
      | O : string * t expr -> t expr
      (* | O : string * 'a T.t expr -> string T.t expr *)
      | I : string * t expr -> t expr
      (* | Version : 'a T.t expr -> unit T.t expr *)
      (* | Pos : string * 'a T.t expr -> string T.t expr *)

    let rec to_spec = trap (function
        | O (fl, expr) -> Ob.([A "-o"; A fl]) @ to_spec expr
        (* | I (fl, expr) -> (Ob.A ("-I " ^ fl)) :: to_spec (Expr expr) *)
        (* | Version expr -> (Ob.A "--version") :: to_spec (Expr expr) *)
        (* | Pos (fl, expr) -> (Ob.A fl) :: to_spec (Expr expr) *)
        (* | End -> [] *)
        (* | Trap (trap, expr, exprs) -> (trap expr) @ (to_spec (Expr exprs)) *)
        | Ptrap (spec, expr) -> spec @ to_spec expr
        | _ -> raise (Fwhale "Unknown ocamlc flag.")
      )

    (* let to_cmd expr = Ob.(Cmd (S ([A "ocamlfind"; A "ocamlc"] @ (to_spec expr)))) *)
  end

  module Ocamlopt = struct
    include (Proc ())

    type _ expr +=
      | O : string * t expr -> t expr
      | I : string * t expr -> t expr

    (* let rec to_spec = function Te x -> (match x with *)
    (*     | O (fl, expr) -> Ob.([A "-o"; A fl]) @ to_spec expr *)
    (*     | _ -> raise (Fwhale "Unknown ocamlc flag.") *)
      (* ) *)
  end


  let test =
    (* let open Expr in *)
    let open Ocamlc in
    (O ("f.out", I ("p/ath", Ret)))

  (* module Ocamlopt = struct *)
  (*   module T = Typ () *)

  (*   type _ expr += *)
  (*     | I : string * 'a T.t expr -> string T.t expr *)
  (*     | End : unit T.t expr *)
  (* end *)



  (* let rec ocamlc_ext = Ocamlc.(function Expr expr -> *)
  (*   begin match expr with *)
  (*     | I (s, rest) -> (Ob.A ("-I " ^ s)) :: ocamlc_ext (Expr rest) *)
  (*     | End -> [] *)
  (*     | _ -> raise Not_found *)
  (*   end) *)

  (* let dsl_main = Ocamlc.( *)
  (*     ocamlc (Expr (O ("test.out", Trap (ocamlc_ext, (Expr (I ("inc/path", End))), End)))) *)
  (*   ) *)

  (* let rec filter_files = function Expr expr -> (match expr with *)
  (*     | Mli (fl, expr) -> (Ob.A ("-o " ^ fl)) :: to_spec (Expr expr) *)
  (*     | End -> [] *)
  (*     | Trap (trap, expr, exprs) -> (trap expr) @ (to_spec (Expr exprs)) *)
  (*     | _ -> raise Not_found *)
  (*   ) *)


  let ls_dir dir =
    let open File in
    let all_files =
      try Sys.readdir dir |> Array.to_list
      with _ -> []
    in
    let files = List.filter_map all_files ~f:(fun path ->
        match extension path with
        | ".mli" -> Some (dir ^ "/" ^ path)
        (* | ".ml" -> Some (Src (Ml.of_path (dir ^ "/" ^ path))) *)
        | _ -> None
      )
    in
    Expr (List.fold_left files ~init:(File.End) ~f:(fun acc path ->
        match extension path with
        | ".mli" -> File.Mli (Mli.of_path path, acc)
        | _ -> assert false
      ))





  module Tools = struct
    module T = Typ ()

    type _ expr +=
      | Ocamlc : eexpr * 'a T.t expr -> (eexpr * eexpr) expr
      | End : unit T.t expr

    let rec to_cmds = function Expr expr -> (match expr with
        | Ocamlc (expr, exprs) -> (Ocamlc.to_cmd expr) :: (to_cmds (Expr exprs))
        | End -> []
        | _ -> raise (Fwhale "Expected tool.")
      )

    let to_seq expr = Ob.Seq (to_cmds expr)
  end

  module Build = struct
    module T = Typ ()

    type ('a, 'b) rule = {
      deps : 'a File.T.t expr;
      prods :  'b File.T.t expr;
      cmds : eexpr;
    }

    type _ expr +=
      | Rule : ('a, 'b) rule * 'c T.t expr -> eexpr T.t expr
      | End : unit T.t expr


    let rec install = function Expr expr -> (match expr with
        | Rule ({deps; prods; cmds}, rule) ->
          Rule.rule ~deps:(File.to_string (Expr deps)) ~prods:(File.to_string (Expr prods)) (fun _ _ ->
              print_endline ("INSTALL " ^ (String.concat ~sep:" " (File.to_string (Expr prods))));
              Tools.to_seq cmds
            );
          install (Expr rule)
        | End -> ()
        (* | Trap (trap, expr, exprs) -> (trap expr) @ (to_spec (Expr exprs)) *)
        | _ -> raise (Fwhale "Expected Rule, or trap, got something else.")
      )

    let install_rules expr =
      Ocamlbuild_plugin.dispatch @@ function
      | Ocamlbuild_plugin.After_rules -> (

          Ocamlbuild_plugin.clear_rules();
          install expr
        )
      | _ -> ()
  end


  module Artifact = struct
    module T = Typ ()

    type _ expr +=
      | Compiled_intf : 'a Build.T.t expr * eexpr -> 'a Build.T.t expr
      | End : unit T.t expr

    (* let build_artifact = function Expr expr -> (match expr with *)
    (*     | Compiled_intf ( *)

    let compile_mli mli_file =
      let open File in
      let cmi_file = typ_conv (module Mli) (module Cmi) mli_file in
      let deps = Mli (mli_file, End) in
      let prods = Cmi (cmi_file, End) in
      let open Build in
      let cmds =
        (Tools.Ocamlc (Ocamlc.(Expr (O ((Cmi.path cmi_file), Pos ((Mli.path mli_file), End)))), Tools.End))
        (* (Ocamlc (Expr (O ("test.out", Trap (ocamlc_ext, (Expr (I ("inc/path", End))), End)))), End) *)
      in
      Build.Rule ({deps; prods; cmds = Expr cmds}, End)
  end




  type _ expr +=
      | Ret : eexpr expr

  type exn += F_whale of eexpr

  let rec build_lib = function Expr expr -> (match expr with
      | File.Mli (f, expr) -> (Expr (Artifact.(Compiled_intf (compile_mli f, Expr expr))))
      | Artifact.Compiled_intf (rule, expr) -> Build.install (Expr rule); build_lib expr
      (* | Ocamlc (expr, exprs) -> (Ocamlc.to_cmd expr) :: (to_cmds (Expr exprs)) *)
      | File.End -> (Expr Ret)
      (* | Artifact.End -> (Expr Artifact.End) *)
      | x -> raise (F_whale (Expr x))
    )

  let is_ret = function Expr expr -> (match expr with
      | Ret  -> true
      | _ -> false
    )




  (* TODO: Check for infinite recursion. Prods = deps will recurse infinitely. *)
  (*       Use a graph to keep track of circular dependecies at each recursion. *)
  let rec build
      ~f
      expr
    =
    if (is_ret expr) then expr
    else build ~f (f expr)


  let lib ~dir =
    Ocamlbuild_plugin.dispatch @@ function
    | Ocamlbuild_plugin.After_rules -> (

        Ocamlbuild_plugin.clear_rules();

        ignore (
          ls_dir dir |>
          build ~f:build_lib
        )
      )
    | _ -> ()
end




(* let to_command spec files = *)
(*   let open Util.Spec in *)
(*   [[Some (A "ocamlfind"); Some (A "ocamlc")]] *)
(*   @[[Some (A "-verbose")]] *)
(*   @spec *)
(*   @[List.map files ~f:(fun file -> Some (A file))] *)
(*   |> specs_to_command *)



  (* let open Ocamlfind in *)
  (* let create (module Common : Ocamlx) = *)
  (*   Common.( *)
  (*     create () |> *)
  (*     set_verbose |> *)
  (*     set_package ~v:findlib_deps |> *)
  (*     set_pathI ~v:[dir] *)
  (*   ) *)
  (* in *)
  (* let ocamlc = create (module Ocamlc) in *)
  (* let ocamlopt = create (module Ocamlopt) in *)

(*
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
        (* ocamldep_sort |> *)
        build t
      )
    | _ -> ()
end
       *)
