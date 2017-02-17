(* open Printf *)
(* open Util *)
(* open Util.Filename *)




(* module Lib = struct *)


(*   (1* xleroy on packed libs and ocamlmklib: *)
(*    * http://caml.inria.fr/pub/ml-archives/caml-list/2002/10/9f4c6dd22941a51b1020132fffcefc2c.en.html *)
(*    * *)
(*    * Packing is a separate step from making a lib. *)
(*    *  ? So, Info should pack, but Lib should be unaware of the packing and just link with the cmo. *)
(*    *  ? We have a separate Compile module. *)
(*    *  *1) *)

(*   module Info = struct *)
(*     type stubs = { *)
(*       o_files : string list; *)
(*       static_lib : string; *)
(*       dynamic_lib : string; *)
(*     } *)

(*     type t = { *)
(*       dir : string; *)
(*       name : string; *)
(*       deps : string list; *)
(*       stubs : stubs option; *)
(*     } *)

(*     type style = [ `Basic | `Pack of string ] *)

(*     type t51 = { *)
(*       dir : string; *)
(*       name : string; *)
(*       files : File.t list; *)
(*       style : style; *)
(*     } *)


(*     let create51 ~lib = *)
(*       let open File in *)
(*       let ordered_names = *)
(*         Tools.run_ocamlfind_ocamldep_sort lib.ml_files |> *)
(*         List.map ~f:Filename.chop_extension in *)
(*       let in_files = *)
(*         (List.map ~f:(create Ml) ordered_names) @ *)
(*         (List.map ~f:(create C) lib.c_files) *)
(*       in *)
(*       { *)
(*         dir = lib.dir; *)
(*         name = lib.name; *)
(*         in_files; *)
(*         out_files; *)
(*       } *)



(*     let input_files t = function *)
(*       | Some filter -> ... *)
(*       | None -> t.files *)


(*     let create ?c_files ~dir ~name ~ml_files ~style = *)
(*       let dir = dirname dir in *)
(*       let stubs = match c_files with *)
(*         | Some files -> Some { *)
(*             o_files = List.map ~f:(replace_suffix_exn ~old:".c" ~new_:".o") files; *)
(*             static_lib = sprintf "%s/lib%s.a" dir name; *)
(*             dynamic_lib = sprintf "%s/lib%s.so" dir name; *)
(*           } *)
(*         | None -> None *)
(*       in *)
(*       let deps = *)
(*         Tools.run_ocamlfind_ocamldep_sort ml_files |> *)
(*         List.map ~f:Filename.chop_extension in *)
(*       {dir; name; deps; stubs} *)

(*     let deps_with_ext {deps;_} ~ext = List.map ~f:(fun dep -> dep ^ ext) deps *)
(*     let bytecode_objs = deps_with_ext ~ext:".cmo" *)
(*     let native_objs = deps_with_ext ~ext:".cmx" *)

(*     let output {dir; name;_} = sprintf "%s/%s" dir name |> Filename.normalize *)
(*     let bytecode_lib {dir; name;_} = sprintf "%s/%s.cma" dir name *)
(*     let native_lib {dir; name;_} = sprintf "%s/%s.cmxa" dir name *)
(*     let shared_lib {dir; name;_} = sprintf "%s/%s.cmxs" dir name *)

(*     let stubs t = *)
(*       match t.stubs with *)
(*       | Some s -> Some (s.o_files, s.static_lib, s.dynamic_lib) *)
(*       | None -> None *)
(*   end *)

(*   (1* TODO: May not be such a great idea to use a record here? *)
(*    *       Since this one structure will be used to generate multiple rules. *1) *)
(*   type ocamlmklib_opts = { *)
(*     o : string; *)
(*     pathL : string list; *)
(*     l : string list; *)
(*   } *)

(*   type t = Info.t * ocamlmklib_opts *)

(*   exception C_stubs_required *)

(*   let stdlib_path = Tools.run_ocamlfind_query "stdlib" *)


(*   let create inf = (inf, { *)
(*       o = Info.output inf; *)
(*       pathL = []; *)
(*       l = [] *)
(*     }) *)


(*   let add_ocamlmklib_l (files, opts) clib = *)
(*     let l = List.cons_uniq clib opts.l in *)
(*     (files, {opts with l}) *)


(*   let add_ocamlmklib_pathL ((files, opts) as t) path = *)
(*     if path = stdlib_path then t else *)
(*     (files, {opts with pathL = List.cons_uniq path opts.pathL}) *)


(*   (1* These -L opts are needed to statically link with stubs of other packages. *1) *)
(*   let link_packages t ~packages = *)
(*     List.map ~f:Tools.run_ocamlfind_query packages |> *)
(*     List.fold_left ~f:add_ocamlmklib_pathL ~init:t *)


(*   let link_clibs t ~clibs = *)
(*     List.fold_left ~f:add_ocamlmklib_l ~init:t clibs *)


(*   (1* Build [.so] and [.a] files. *1) *)
(*   (1* TODO: Warn on missing flags that are likely to break the library. *1) *)
(*   let install_stub_lib_rules (inf, {pathL; l; o}) = *)
(*     match Info.stubs inf with *)
(*     | Some (o_files, a, dll) -> *)
(*       Rule.rule ~deps:o_files ~prods:[a; dll] (fun _ _ -> *)
(*           Tools.ocamlmklib ~o ~pathL ~l o_files *)
(*         ) *)
(*     | None -> raise C_stubs_required *)


(*   let install_ml_lib_rules (inf, {pathL; l; o}) ~objs ~prods = *)
(*     let deps = match Info.stubs inf with *)
(*       | Some (_, a, dll) -> [a; dll]@objs *)
(*       | None -> objs *)
(*     in *)
(*     Rule.rule ~deps ~prods (fun _ _ -> *)
(*         Tools.ocamlmklib ~verbose:() ~o ~pathL ~l objs *)
(*       ) *)


(*   let install_bytecode_lib_rules ((inf,_) as t) = *)
(*     install_ml_lib_rules t ~objs:(Info.bytecode_objs inf) ~prods:[Info.bytecode_lib inf] *)


(*   let install_native_lib_rules ((inf,_) as t) = *)
(*     install_ml_lib_rules t ~objs:(Info.native_objs inf) ~prods:[Info.native_lib inf] *)

(*   (1* TODO: install_shared_lib_rules *)
(*    *  - For "native plugins" .cmxs we may need to build using ocamlc directly. *)
(*    *    Or, pass through extra ocamlopt arguments to ocamlmklib, although *)
(*    *    this arguably adds an unnecessary level of indirection. *1) *)
(* end *)
