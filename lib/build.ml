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

(* Extensible domain specific languages. *)
module Dsl = struct
  module Expr (M : sig type ret end) () = struct
    (* module type Inst = sig *)
    (*   type t *)
    (*   val eval : M.ret -> t -> M.ret *)
    (*   val expr : t *)
    (* end *)

    type expr = ..
    type t = (M.ret -> M.ret) * expr

    module type Ext = sig
      type t
      val eval : M.ret -> t -> M.ret
    end

    module Extend (E : Ext) : sig
      type expr += T of E.t
      val expr : E.t -> t
    end = struct
      type expr += T of E.t

      (* let make_expr *)
      (*     (type a) *)
      (*     (module E : Ext with type t = a) *)
      (*     (x : a) *)
      (*   = *)
      (*   (module struct *)
      (*     type t = E.t *)
      (*     let eval = E.eval *)
      (*     let expr = x *)
      (*   end : Inst) *)

      (* Lazy fn (or Lazy.t) instead of the first class module
       * since we basically need just one fn call? *)
      let eval acc ~t = E.eval acc t
      let expr t = (eval ~t, T t)
    end

    let eval = fun xs ->
      List.fold_left xs ~f:(fun acc (eval, _) ->
          eval acc
        )

    let xs = List.map ~f:(fun (_, x) -> x)
    let iter exprs = List.iter (xs exprs)
    (* let fold_left exprs = List.fold_left (xs exprs) *)
  end

  (* Example *)


    module L = Expr (struct
        type ret = int list
      end) ()


    module Ext1 = struct
      module M = struct
        type t =
          | A of string
          | B of float

        let eval acc = function
          | A _ -> 1 :: acc
          | B _ -> 2 :: acc
      end

      include L.Extend (M)
      include M
    end


    module Ext2 = struct
      module M = struct
        type t =
          | Z

        let eval acc = function
          | Z -> 0 :: acc
      end

      include L.Extend (M)
      include M
    end


    let strip =
      let expr = [Ext1.(expr (A "s")); Ext2.(expr Z)] in
      (* Iteration and pattern matching. *)
      List.iter expr ~f:(function
          | (_, Ext1.T x) -> Ext1.(match x with
              | A _ -> ()
              | B _ -> ()
            )
          | _ -> ()
        );
      (* Modification. Inserting an expression. *)
      let expr2 = List.fold_left expr ~init:[] ~f:(fun acc ((_, x) as expr) -> match x with
          | Ext1.T (Ext1.A _) -> expr :: (Ext2.(expr Z) :: acc)
          | _ -> expr :: acc
        ) in
      L.eval ~init:[] expr2

end


module Build = struct
  include Dsl

  type exn += F_whale of string
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

    type t = ..
    type t +=
      | Cmi : Cmi.t -> t
      | Mli : Mli.t -> t

    let to_string trap acc = function
        | Cmi f -> (Cmi.path f) :: acc
        | Mli f -> (Mli.path f) :: acc
        | a -> trap acc a

    (*
    let ls_dir dir =
      let open File in
      let all_files =
        try Sys.readdir dir |> Array.to_list
        with _ -> []
      in
      List.filter_map all_files ~f:(fun path ->
          match extension path with
          | ".mli" -> Some (File.Mli (Mli.of_path path))
          (* | ".ml" -> Some (Src (Ml.of_path (dir ^ "/" ^ path))) *)
          | _ -> None
        )
     *)
  end

  module Tools = struct
    module L = Expr (struct type ret = Ob.command list end) ()

    (* module Tool (T : L.T) () = struct *)
    (*   module L = Expr (struct type ret = Ob.spec list end) () *)

    (*   let to_bin_specs bin f flags = *)
    (*     Ob.A bin :: (List.fold_left ~init:[] ~f:f flags) *)
    (* end *)

    let to_bin_specs bin f flags =
      Ob.A bin :: (List.fold_left ~init:[] ~f:f flags)

    module Compiler = struct
      type t =
        | Version
        | Verbose

      let eval acc = function
        | Version ->  Ob.A "-version" :: acc
        | Verbose ->  Ob.A "-verbose" :: acc
    end


    module Ocamlc = struct
      module L = Expr (struct type ret = Ob.spec list end) ()

      module M = struct
        type t =
          | O of string
          | I of string

        let eval acc = function
          | O v -> Ob.([A "-o"; A v]) @ acc
          | I v -> Ob.([A "-I"; A v]) @ acc
      end

      include L.Extend (M)
      include M

      let to_specs = to_bin_specs "ocamlc" eval
    end


    let f =
      let g = Ocamlopt.(O "test") in
      Ocamlc.eval [] g

    (* module Ocamlopt = struct *)
    (*   include Compiler () *)

    (*   type _ L.expr += *)
    (*     | O : string -> t L.expr *)
    (*     | I : string -> t L.expr *)

    (*   let to_specs = *)
    (*     to_bin_specs "ocamlopt" (fun acc -> function *)
    (*         | O v -> Ob.([A "-o"; A v]) @ acc *)
    (*         | I v -> Ob.([A "-I"; A v]) @ acc *)
    (*         | a -> to_specs acc a *)
    (*       ) *)
    (* end *)

    (*
    module Ocamlfind = struct
      (* include (Tool (Spec_list) ()) *)

      module T = (L.Typ (Spec_list) ())

      module M = struct
        type t =
          | Package of string list
          | Ocamlc of Ocamlc.t L.expr list

        let eval =
          to_bin_specs "ocamlfind" (fun acc -> function
              | Ocamlc a -> Ocamlc.to_specs a @ acc
              | Package _ -> acc
              (* | a -> eval a @ acc *)
            )
      end

      (* include (T.Extend (M)) *)
    end
    *)

    module L = Expr (struct type ret = Ob.command list end) ()
    (* module Build = struct *)
    (*   module Typ = struct *)
    (*     type t = Ob.command list *)
    (*   end *)

    (*   include (L.Typ (Typ) ()) *)

    (*   type _ L.expr += *)
    (*     | Ocamlc : Ocamlc.t L.expr list -> t L.expr *)
    (*     | Ocamlfind : Ocamlfind.t L.expr list -> t L.expr *)


    (*   let to_cmds acc = function *)
    (*     | Ocamlc a -> Ob.Cmd (Ob.S (Ocamlc.to_specs a)) :: acc *)
    (*     | Ocamlfind a -> Ob.Cmd (Ob.S (Ocamlfind.to_specs a)) :: acc *)
    (*     | a -> eval a @ acc *)


    (*   let to_seq t = Ob.Seq (List.fold_left ~init:[] ~f:to_cmds t) *)
    (* end *)
  end


  (* New tool *)
  (* module Ar = struct *)
  (*   include Tools.Tool *)

  (* end *)

  (* module Ar = struct *)
  (*   module Ar = struct *)
  (*     include Tools.Tool *)

  (*     type expr += *)
  (*       | X *)
  (*       | T *)

  (*     let to_spec = *)
  (*       bin_spec "ar" (fun acc -> function *)
  (*         | X -> (Ob.A "-x") :: acc *)
  (*         | a -> tool_spec acc a *)
  (*       ) *)
  (*   end *)

  (*   type Tools.expr += *)
  (*     | Ar of Ar.expr list *)
  (* end *)


  (* Extension *)
  module Ocamlc_ext = struct
    open Tools


    module M = struct
      type t =
        | Z

      let eval = List.fold_left ~init:[] ~f:(fun acc -> function
          | Z -> Ob.A "-z" :: acc
        )
    end

    module T = L.Extend (Spec_list) (M) ()

    include M
    include T

    (* type _ L.expr += *)
    (*   | Z : t L.expr *)

    (* let to_spec acc = Ocamlc.T.Extend(function *)
    (*   | Z -> Ob.A "-z" :: acc *)
    (*   | a -> eval acc a *)
  end

  (* Extension of an extension *)


  let test =
    (* let open Tools.Build in *)
    Tools.Ocamlc.([O "test.out"; I "p/ath"; Ocamlc_ext.(ext [Z])])

(*
  module Build = struct
    module T = Typ ()

    type rule = {
      deps : File.t list;
      prods :  File.t list;
      cmds : Command.t list;
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


  module Lib = struct
    type t = ..
    type t +=
      | Compiled_intf : Build.rule -> t

    let compile_mli mli_file =
      let open File in
      let cmi_file = typ_conv (module Mli) (module Cmi) mli_file in
      let deps = [Mli mli_file] in
      let prods = [Cmi cmi_file] in
      let cmds =
        (Tools.Ocamlc (Ocamlc.(Expr (O ((Cmi.path cmi_file), Pos ((Mli.path mli_file), End)))), Tools.End))
        (* (Ocamlc (Expr (O ("test.out", Trap (ocamlc_ext, (Expr (I ("inc/path", End))), End)))), End) *)
      in
      Build.Rule ({deps; prods; cmds}, End)

    let build_files trap acc = File.(function
        | Mli f -> Compiled_intf (compile_mli f)
      )

    let is_rule = function
      | Build.Rule _ -> true
      | _ -> false

    let rec build_lib = function Expr expr -> (match expr with
        | Artifact.Compiled_intf (rule, expr) -> Build.install (Expr rule); build_lib expr
        (* | Ocamlc (expr, exprs) -> (Ocamlc.to_cmd expr) :: (to_cmds (Expr exprs)) *)
        | File.End -> (Expr Ret)
        (* | Artifact.End -> (Expr Artifact.End) *)
        | x -> raise (F_whale (Expr x))
      )

    (* let create files = *)
    (*   Dsl.run_self kern files *)


  end






  let lib ~dir =
    Ocamlbuild_plugin.dispatch @@ function
    | Ocamlbuild_plugin.After_rules -> (

        Ocamlbuild_plugin.clear_rules();

        ignore (
          ls_dir dir |>
          Dsl.run_self Lib.kern Lib.stop
        )
      )
    | _ -> ()
           *)
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
