open Ocamlbuild_plugin
open Util
open Printf


module Tool : sig
  type t
  val create : unit -> t
  val unit_flag : string -> (t -> unit option) * (t -> unit -> t)
  val string_flag : delim:[`None | `Space | `Equal] -> string -> (t -> string option) * (t -> string -> t)
  val string_list_flag : delim:[`None | `Space | `Equal] -> string -> (t -> string list option) * (t -> string list -> t)
  val flags : t -> spec list
end =
struct
  type flag_val =
    | Unit of spec list
    | String of string * (string -> spec list)
    | String_list of string list * (string list -> spec list)

  type t = (string * flag_val) list

  let create () = []

  let string_to_spec ~delim name value =
    match delim with
    | `Space -> [A name; A value]
    | `None -> [A (name ^ value)]
    | `Equal -> [A (sprintf "%s=%s" name value)]

  let string_list_to_spec ~delim name value =
      List.map value ~f:(fun x ->
        string_to_spec ~delim name x
      ) |>
      List.flatten

  let flag_val ~name ~of_flag t =
    match List.Assoc.find t name with
    | Some flag -> of_flag flag
    | None -> None

  let set_flag_val ~name ~to_flag t value =
    let t = List.remove_assoc name t in
    let flag = to_flag name value in
    (name, flag) :: t

  let unit_flag name =
    let to_flag = fun name _ -> Unit [A name] in
    let of_flag = function Unit _ -> Some () | _ -> assert false in
    let get = flag_val ~name ~of_flag in
    let set = set_flag_val ~name ~to_flag in
    (get, set)

  let string_flag ~delim name =
    let to_flag = fun name value -> String (value, string_to_spec ~delim name) in
    let of_flag = function String (s, _) -> Some s | _ -> assert false in
    let get = flag_val ~name ~of_flag in
    let set = set_flag_val ~name ~to_flag in
    (get, set)

  let string_list_flag ~delim name =
    let to_flag = fun name value -> String_list (value, string_list_to_spec ~delim name) in
    let of_flag = function String_list (lis, _) -> Some lis | _ -> assert false in
    let get = flag_val ~name ~of_flag in
    let set = set_flag_val ~name ~to_flag in
    (get, set)

  let flags t =
    List.fold_left t ~init:[] ~f:(fun acc (_, flag) ->
        begin match flag with
          | Unit spec -> spec
          | String (s, to_spec) -> to_spec s
          | String_list (lis, to_spec) -> to_spec lis
        end :: acc
      )
    |> List.flatten
end


module Ocaml_tool = struct
  include Tool

  let verbose, set_verbose = unit_flag "-verbose"
end


module Ocamlc = struct
  include Ocaml_tool

  let to_spec t = (A "ocamlc") :: (flags t)

  let a, set_a = unit_flag "-a"
  let c, set_c = unit_flag "-c"
  let o, set_o = string_flag ~delim:`Space "-o"
  let pathI, set_pathI = string_list_flag ~delim:`Space "-I"
end


module Ocamlfind_ocamlc = struct
  include Ocamlc

  let to_spec t = (A "ocamlfind") :: (Ocamlc.to_spec t)

  let package, set_package = string_list_flag ~delim:`Space "-package"
  let linkpkg, set_linkpkg = unit_flag "-linkpkg"
end
