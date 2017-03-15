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
  (* Extensible expression vocabulary. *)
  type _ expr = ..

  (* Non-unifying type minter. *)
  module Typ () = struct
    type 'a a
    type 'a t = 'a a
  end

  (* Usage example, also bootsraps the language. *)

  (* Define new type *)
  module Lang = Typ ()

  (* Define the root expression.
   * Everything else in the language is namespaced using modules. *)
  (* type _ expr += Exec : ('a * 'b Lang.t) expr -> ('a * 'b Lang.t) expr *)

  (* Polymorphic linked list ['a; 'b; 'c;..] of other expressions. *)

  (* type ('a, 'b) boot = ('a expr -> 'b) * 'a expr *)
  (* type eexpr = Eexpr : 'a expr -> eexpr *)

  (* type _ exprs = *)
  (* | Expr : eexpr * 'b exprs -> (eexpr * 'b exprs) exprs *)
  (* | Exit : unit exprs *)

  (* Executor, rec funs for funs.
   *
   * Everything in the language is implemented using extensions,
   * which are nothing more than function matching on an expression.
   * *)
  (* let rec exec : type a b . *)
  (*   (eexpr -> a) -> b exprs -> unit *)
  (*   = *)
  (*   fun kern exprs -> *)
  (*     match exprs with *)
  (*     | Expr (expr, exprs) -> *)
  (*       ignore (kern expr); *)
  (*       ignore (exec kern exprs) *)
  (*     | Exit -> () *)


  (* Sample extension *)

  (* let kern = function *)
  (*   | Time -> print_endline ("TIME IS:" ^ (string_of_float (Sys.time ()))) *)
  (*   | _ -> raise Not_found *)

  (* let kern2 = function *)
  (*   | Eexpr (Time as b) -> kern b *)
  (*   | _ -> raise Not_found *)

  type eexpr = Expr : 'a expr -> eexpr

  (* Core expressions. *)
  (* type _ exprs = *)
  (*   (1* | Exec : 'a expr -> unit expr *1) *)
  (*   | Exec : eexpr * 'b exprs -> ('a expr * 'b exprs) exprs *)
  (*   | Exit : unit exprs *)

  type ret = Ret : 'a -> ret

  type _ expr +=
    (* | Expr : 'a expr -> eexpr expr *)
    | Exit : unit expr
    (* | Ret : 'a -> unit expr *)

  module T = Typ ()
  type _ expr +=
    | Ocamlc : 'a T.t expr -> 'a T.t expr
    | O : string * 'a T.t expr -> string T.t expr
    | I : string * 'a T.t expr -> string T.t expr
    | Version : 'a T.t expr -> unit T.t expr
    | End : unit T.t expr (* Empty tail constructor. *)

  open Ocamlbuild_plugin

  (* module Variant3 = struct *)
  (*   type _ expr += *)
  (*     | X : ('a expr -> (eexpr -> ret) -> ret) * 'a expr -> 'b expr *)

  (*   let rec eval : *)
  (*     eexpr -> ret *)
  (*     = function *)
  (*     | Expr expr -> *)
  (*        begin match expr with *)
  (*        | X (kernel, exprs) -> Ret (kernel exprs eval) *)
  (*        | _ -> raise Not_found *)
  (*        end *)
  (*     (1* | _ -> raise Not_found *1) *)

  (*   let rec ocamlc : type a. *)
  (*     a expr -> (eexpr -> spec list) -> ret *)
  (*     = *)
  (*     fun expr eval -> *)
  (*     match expr with *)
  (*     | O (s, rest) -> Ret (A ("-o " ^ s) :: (match eval (Expr rest) with Ret r -> r)) *)
  (*     | End -> Ret [] *)
  (*     | a -> eval (Expr a) *)

  (*   (1* let rec ocamlcI : type a . a expr -> spec list *1) *)
  (*   (1*   = function *1) *)
  (*   (1*   | O (s, rest) -> A ("-o " ^ s) :: ocamlc rest *1) *)
  (*   (1*   | End -> [] *1) *)
  (*   (1*   | _ -> raise Not_found *1) *)

  (*   let main = *)
  (*     Expr (X (ocamlc, O ("test.out", End))) *)
  (*   (1* let kern1 = function *1) *)
  (*   (1*   | Expr (Ocamlc expr) -> ocamlc expr *1) *)
  (*   (1*   | es -> es *1) *)

  (* end *)

  (* An Ocaml unix, get it??? *)
  module Onix = struct

    type eexpr = Eexpr : 'a expr -> eexpr

    type _ expr +=
      | X : ('a expr -> 'b expr) * 'a expr -> (('a expr -> 'b expr) * 'a expr) expr
      | Ret : ret expr

    let rec kern : type a b .
      a expr -> b expr
      =
      fun expr ->
        begin match expr with
          | X (proc, expr, next_expr) -> kern (proc expr)
          (* | Ret as r -> *)
          | _ -> raise Not_found
        end

    (* type _ kern = *)
    (*   | Proc : 'a expr -> sh expr *)

    (* let rec eval : type a b . *)
    (*   a expr -> b sh -> ret *)
    (*   = *)
    (*   fun exprs -> function *)
    (*     | Pipe (from, to_) -> eval (from exprs) to_ *)
    (*     | End -> exprs *)

    (* type _ expr += *)
    (*   | Lis : 'a * 'b expr -> 'a expr *)
      (* List.fold_left ~init:(Ret 0) *)

    let rec ocamlc : type a .
      (eexpr -> spec list) -> a expr -> spec list
      =
      fun eval -> function
      | O (s, rest) -> A ("-o " ^ s) :: ocamlc eval rest
      | End -> []
      | expr -> eval (Eexpr expr)


    let rec extension : type a .
      (eexpr -> spec list) -> a expr -> spec list
      =
      fun eval -> function
      | I (s, rest) -> A ("-I " ^ s) :: extension eval rest
      | expr -> eval (Eexpr expr)


    let dsl_main =
      eval
      Exec (ocamlc, O ("test.out", End), Ret)
  end


  (* module Variant1 = struct *)

  (*   type eexpr = Expr : 'a expr -> eexpr *)

  (*   type _ kerns = *)
  (*     | Kern : (eexpr -> eexpr) * 'c kerns -> ((eexpr -> eexpr) * 'c) kerns *)
  (*     | End : unit kerns *)

  (*   let rec eval : type a . *)
  (*     a kerns -> eexpr -> eexpr *)
  (*     = *)
  (*     fun kerns exprs -> *)
  (*       match kerns with *)
  (*       | Kern (k, ks) -> eval ks (k exprs) *)
  (*       | End -> exprs *)

  (*   type _ expr += *)
  (*     | Lis : 'a * eexpr -> 'a expr *)
  (*     (1* List.fold_left ~init:(Ret 0) *1) *)

  (*   let rec ocamlc : type a . a expr -> eexpr *)
  (*     = function *)
  (*     | O (s, rest) -> Expr (Lis (A ("-o " ^ s), ocamlc rest)) *)
  (*     | e -> Expr e *)

  (*   let kern1 = function *)
  (*     | Expr (Ocamlc expr) -> ocamlc expr *)
  (*     | es -> es *)
  (* end *)


    (* exec kern (Expr (Eexpr Time, Exit)) *)

  (* let rec build *)
  (*     ?(is_target=(List.for_all ~f:is_rule)) *)
  (*     ?(init=[]) *)
  (*     ~f *)
  (*     deps *)
  (*   = *)
  (*   if (is_target deps) then deps *)
  (*   else build ~is_target ~f (List.fold_left ~init ~f deps) *)


end


module Build = struct
  include Dsl

  type _ expr +=
    | Rule : 'a * 'b * 'c expr -> ('a * 'b * 'c) expr

  (* let install_rules rules = *)
  (*   Ocamlbuild_plugin.dispatch @@ function *)
  (*   | Ocamlbuild_plugin.After_rules -> ( *)

  (*       Ocamlbuild_plugin.clear_rules(); *)

  (*       List.filter_map ~f:(function Rule r -> Some r | _ -> None) rules |> *)
  (*       List.iter ~f:(fun {deps; prods; cmds} -> *)
  (*           Rule.rule ~deps ~prods (fun _ _ -> *)


  (*               (1* get tool from cmd variant *1) *)


  (*               List.map cmds ~f:(function *)
  (*                   | (t : cmd) -> Tool.to_spec t *)

  (*                   (1* | a -> a *1) *)
  (*                 ) *)
  (*             ) *)
  (*         ) *)
  (*     ) *)
  (*   | _ -> () *)


  (* TODO: Check for infinite recursion. Prods = deps will recurse infinitely. *)
  (*       Use a graph to keep track of circular dependecies at each recursion. *)
  (* let rec build *)
  (*     ?(is_target=(List.for_all ~f:is_rule)) *)
  (*     ?(init=[]) *)
  (*     ~f *)
  (*     deps *)
  (*   = *)
  (*   if (is_target deps) then deps *)
  (*   else build ~is_target ~f (List.fold_left ~init ~f deps) *)
end


module Build_ocaml = struct
  include Build

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

  module Ocamlc = struct
    module T = Typ ()

    type _ expr +=
      | O : string * 'a T.t expr -> string T.t expr
      | I : string * 'a T.t expr -> string T.t expr
      | Version : 'a T.t expr -> unit T.t expr
      | End : unit T.t expr (* Empty tail constructor. *)
  end

  module Ocamlopt = struct
    module T = Typ ()

    type _ expr +=
      | I : string * 'a T.t expr -> string T.t expr
      | End : unit T.t expr
  end

  module Artifact = struct
    module T = Typ ()

    type _ expr +=
      | Compiled_interface : File.Cmi.t -> File.Cmi.t expr
  end
end


(* let test = *)
(*   let open Build_ocaml in *)
(*   let expr = Exec (Std.Expr (Ocamlc.(Version End),Std.End)) in *)
(*   Std.Expr (Ocamlc.(O ("file.out", I ("p/a/t/h", I ("other/p/a/t/h", End)))), Std.End) *)


(*   let cmi_file = typ_conv (module Mli) (module Cmi) mli_file in *)

(* let to_command spec files = *)
(*   let open Util.Spec in *)
(*   [[Some (A "ocamlfind"); Some (A "ocamlc")]] *)
(*   @[[Some (A "-verbose")]] *)
(*   @spec *)
(*   @[List.map files ~f:(fun file -> Some (A file))] *)
(*   |> specs_to_command *)


(* let ls_dir dir = *)
(*   let open File in *)
(*   let all_files = *)
(*     try Sys.readdir dir |> Array.to_list *)
(*     with _ -> [] *)
(*   in *)
(*   List.filter_map all_files ~f:(fun path -> *)
(*       match extension path with *)
(*       | ".mli" -> Some (Intf (Mli.of_path (dir ^ "/" ^ path))) *)
(*       (1* | ".ml" -> Some (Src (Ml.of_path (dir ^ "/" ^ path))) *1) *)
(*       | _ -> None *)
(*     ) *)


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
