open Ocamlbuild_plugin
open Util
open Printf


module type Tool = sig
  type t
  val create : unit -> t
  val unit_flag : string -> (t -> unit option) * (t -> t)
  val string_flag : delim:[`None | `Space | `Equal] -> string -> (t -> string option) * (v:string -> t -> t)
  val string_list_flag : delim:[`None | `Space | `Equal] -> string -> (t -> string list option) * (v:string list -> t -> t)
  val flags : t -> spec list
  val to_spec : t -> spec list
end

module Tool : Tool = struct
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

  let set_flag_val ~name ~to_flag ~v t =
    let t = List.remove_assoc name t in
    let flag = to_flag name v in
    (name, flag) :: t

  let unit_flag name =
    let to_flag = fun name _ -> Unit [A name] in
    let of_flag = function Unit _ -> Some () | _ -> assert false in
    let get = flag_val ~name ~of_flag in
    let set = set_flag_val ~name ~to_flag ~v:() in
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

  let to_spec = flags
end


module Ocamlx = struct
  include Tool

  let a, set_a = unit_flag "-a"
  let c, set_c = unit_flag "-c"
  let o, set_o = string_flag ~delim:`Space "-o"
  let pathI, set_pathI = string_list_flag ~delim:`Space "-I"
  let verbose, set_verbose = unit_flag "-verbose"
end


module Ocamlc = struct
  include Ocamlx

  let to_spec t = (A "ocamlc") :: (flags t)
end


module Ocamlopt = struct
  include Ocamlx

  let to_spec t = (A "ocamlc") :: (flags t)
end


module Ocamlfind = struct
  (* module type S = sig *)
  (*   type t *)
  (*   val package : t -> string list option *)
  (*   val set_package : v:string list -> t -> t *)
  (*   val linkpkg : v:unit option -> t *)
  (*   val set_linkpkg : t -> t *)
  (*   val to_spec : t -> spec list *)
  (* end *)

  module Make (T : Tool) (*: S with type t := T.t*) = struct
    open T

    let to_spec t = (A "ocamlfind") :: (T.to_spec t)

    let package, set_package = string_list_flag ~delim:`Space "-package"
    let linkpkg, set_linkpkg = unit_flag "-linkpkg"
  end

  module Ocamlx = struct
    include Ocamlx
    include Make(Ocamlx)
  end
  module type Ocamlx = module type of Ocamlx

  module Ocamlc = struct
    include Ocamlc
    include Make(Ocamlc)
  end

  module Ocamlopt = struct
    include Ocamlopt
    include Make(Ocamlopt)
  end
end
