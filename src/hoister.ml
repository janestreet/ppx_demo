open! Core
open Ppxlib

module Key = struct
  module T = struct
    type t =
      | Variable of Name.Hoisted.t
      | Module of Name.Hoisted.t
    [@@deriving sexp, equal, compare, hash]
  end

  include T
  include Comparable.Make (T)
  module Hash_queue = Hash_queue.Make (T)
end

module Hoist_as = struct
  type t =
    | Value of
        { demo_string : string
        ; hoisted_name : Name.Hoisted.t
        }
    | Module of
        { demo_string : string
        ; hoisted_name : Name.Hoisted.t
        }

  let create ~name:hoisted_name ~demo_string =
    (* If the name is capitalized, we will hoist the demo string within a module.
       If not, we will hoist it as a value *)
    match Name.Hoisted.is_capitalized hoisted_name with
    | true -> Module { demo_string; hoisted_name }
    | false -> Value { demo_string; hoisted_name }
  ;;

  let to_queue_key = function
    | Value { hoisted_name; _ } -> Key.Variable hoisted_name
    | Module { hoisted_name; _ } -> Module hoisted_name
  ;;
end

(* Using a Hash_queue to maintain the order of the structure items to match the order in
   which the associated variables/modules were defined. Does it really matter? Maybe for
   shadowing purposes.
*)
let registry : (Key.t, structure_item) Hash_queue.t = Key.Hash_queue.create ()

let register_demo_string : loc:location -> names:Name.t -> string -> unit =
  fun ~loc ~names demo_string ->
  let t =
    let hoisted_name =
      match names with
      | Name.No_original hoisted_name -> hoisted_name
      | Original_and_hoisted (_, Some hoisted_name) -> hoisted_name
      | Original_and_hoisted (original_name, None) ->
        (* If no hoisted override is given, we use the original name *)
        Name.Original.to_hoisted original_name
    in
    Hoist_as.create ~name:hoisted_name ~demo_string
  in
  let open (val Ast_builder.make loc) in
  let structure_item =
    match t with
    | Module { hoisted_name; demo_string; _ } ->
      let modules =
        pmod_structure [ [%stri let ppx_demo_string = [%e estring demo_string]] ]
      in
      pstr_module
        (module_binding
           ~name:(Located.mk (Some (Name.Hoisted.to_string hoisted_name)))
           ~expr:modules)
    | Value { hoisted_name; demo_string; _ } ->
      [%stri
        let [%p ppat_var (Located.mk (Name.Hoisted.to_string hoisted_name))] =
          [%e estring demo_string]
        ;;]
  in
  let key = Hoist_as.to_queue_key t in
  match Hash_queue.enqueue_back registry key structure_item with
  | `Ok -> ()
  | `Key_already_present ->
    let module_or_var_string, hoisted_name =
      match key with
      | Module hoisted_name -> "module", Name.Hoisted.to_string hoisted_name
      | Variable hoisted_name -> "variable", Name.Hoisted.to_string hoisted_name
    in
    Location.raise_errorf
      ~loc
      "A %s with the name '%s' has already been hoisted. Please choose a different name \
       for the variable, or add a unique name attribute to the ppx expression (e.g. \
       [@name \"unique_name\"])."
      module_or_var_string
      hoisted_name
;;

let create_hoisted_module ~loc =
  let open (val Ast_builder.make loc) in
  let modules = Hash_queue.to_list registry |> pmod_structure in
  let module_ =
    pstr_module (module_binding ~name:(Located.mk (Some "Ppx_demo_hoist")) ~expr:modules)
  in
  [%stri
    open struct
      (* This warning is at the `open struct` level, so it should not affect the warnings
     thrown by the values within the module itself. We've also added error throwing that's
     a bit more specific within the lazy sheets code, so this should be safe to do *)
      [@@@ocaml.warning "-60"]

      [%%i module_]
    end]
;;

let is_empty () = Hash_queue.is_empty registry
