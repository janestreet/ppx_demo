open! Core
open Ppxlib
open Ast_builder.Default

let ppx_name = "demo"
let hoisted_ppx_name = "demo_hoist"
let bonsai_docs_ppx_name = "demo_md"
let ghost_loc loc = { loc with loc_ghost = true }

let ghosted_extension_loc ctxt =
  Expansion_context.Extension.extension_point_loc ctxt |> ghost_loc
;;

let get_regex kind =
  let name =
    match kind with
    (* Match either %demo or %demo_hoist depending on the [kind]. Allow for leading and
       trailing spaces. We also allow for [%%demo_hoist *)
    | `Hoisted -> {%string|%?%{hoisted_ppx_name}|}
    | `Unhoisted -> ppx_name
  in
  let name_attr_pattern =
    {%string|(?P<name_attr>\s*\[@name\s*"%{Name.ocaml_identifier_regex_string}"\s*\]\s*)?|}
  in
  Re2.create_exn
    ~options:{ Re2.Options.default with dot_nl = true }
    {%string|^(?:\s*\[%%{name})(?P<content>.*?)%{name_attr_pattern}\]\s*$|}
;;

let match_regex ~loc ~kind file_contents =
  let regex = get_regex kind in
  let () =
    (* Disallow [%demo_hoist ... [@name ""]] syntax. This is because the attribute is
       actually attached to the final expression in the block, when we want it to be attached
       to the entire thing.

       This will not always raise when the user uses the [%demo_hoist ...] syntax. It will
       only raise if the codepath has entered the fallthrough branch
    *)
    match Re2.find_all ~sub:(`Name "name_attr") regex file_contents with
    | Ok [] | Error _ -> ()
    | Ok _ ->
      Location.raise_errorf
        ~loc
        "[@name] attribute not allowed when using [%s ...] syntax"
        "%demo_hoist"
  in
  Re2.find_all ~sub:(`Name "content") regex file_contents
;;

(** [extract_extension_content] should only be called where we expect [ [%demo ...] ] or
    [ [%demo_hoist ... ] ] blocks in expression positions. *)
let extract_extension_content ~loc ~(kind : [ `Hoisted | `Unhoisted ]) file_contents =
  match match_regex ~loc ~kind file_contents with
  | Ok [ match_ ] -> match_
  | Ok _ ->
    Location.raise_errorf
      ~loc
      "Regex unexpectedly matched more than one time. This is an error within ppx_demo, \
       please contact the maintainers."
  | Error _ ->
    Location.raise_errorf
      ~loc
      "Unable to match regex to file contents. This might be an error within ppx_demo, \
       please contact the maintainers."
;;

let files = String.Table.create ()

let read_file filename =
  match Hashtbl.find files filename with
  | Some file_contents -> file_contents
  | None ->
    let file_contents = In_channel.read_all filename in
    Hashtbl.add_exn files ~key:filename ~data:file_contents;
    file_contents
;;

let extract_string_from ~loc =
  read_file loc.loc_start.pos_fname
  |> String.sub
       ~pos:loc.loc_start.pos_cnum
       ~len:(loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
;;

let get_name_from_attributes : loc:location -> attributes -> Name.Hoisted.t option =
  fun ~loc -> function
  | [] -> None
  | [ ({ attr_name = { txt = "name"; _ }
       ; attr_payload =
           PStr
             [ { pstr_desc =
                   Pstr_eval
                     ( { pexp_desc = Pexp_constant (Pconst_string (hoisted_name, _, _))
                       ; _
                       }
                     , _ )
               ; _
               }
             ]
       ; _
       } as attribute)
    ] ->
    Ppxlib.Attribute.mark_as_handled_manually attribute;
    Name.Hoisted.of_string ~loc hoisted_name |> Option.return
  | _ ->
    Location.raise_errorf
      ~loc
      "Incorrect attributes applied to ppx_demo. Only a single, optional 'name' \
       attribute is allowed."
;;

module Demo = struct
  (* This GADT is used to ensure the input and return type of the [expand] function.
     It can be removed if necessary
  *)
  type 'a t =
    | Expression : expression -> expression t
    | Module_expr : structure -> module_expr t

  let create_demo_string ~loc =
    extract_string_from ~loc
    |> extract_extension_content ~loc ~kind:`Unhoisted
    |> Dedent.string
  ;;

  let expand : type a. loc:location -> a t -> a =
    fun ~loc expr_or_structure ->
    let string = create_demo_string ~loc in
    match expr_or_structure with
    | Expression expression -> pexp_tuple ~loc [ expression; estring ~loc string ]
    | Module_expr structure ->
      (* let ppx_demo_string = (* demo string of structure *) *)
      let demo =
        pstr_value
          ~loc
          Nonrecursive
          [ value_binding
              ~loc
              ~pat:(ppat_var ~loc { txt = "ppx_demo_string"; loc })
              ~expr:(estring ~loc string)
          ]
      in
      (* {[
     struct
       (* original structure *)

       let ppx_demo_string = (* demo string of structure *)
     end
     ]}
      *)
      pmod_structure ~loc (structure @ [ demo ])
  ;;

  let extensions =
    [ Extension.V3.declare
        ppx_name
        Expression
        Ast_pattern.(single_expr_payload __)
        (fun ~ctxt expr ->
          let loc = ghosted_extension_loc ctxt in
          expand ~loc (Expression expr))
    ; Extension.V3.declare
        ppx_name
        Module_expr
        Ast_pattern.(pstr __)
        (fun ~ctxt expr ->
          let loc = ghosted_extension_loc ctxt in
          expand ~loc (Module_expr expr))
    ]
  ;;
end

module Demo_hoist = struct
  (* This GADT is used to ensure the input and return type of the [expand] function.
     It can be removed if necessary
  *)
  type 'a t =
    | Expression : expression -> expression t
    | Structure_item : structure_item -> structure_item t

  let create_demo_string ~loc = extract_string_from ~loc |> Dedent.string

  let get_names_from_value_binding =
    let raise_unit_error ~loc =
      Location.raise_errorf
        ~loc
        "This let%s expression is bound to [unit]. Please add a name attr or give the \
         variable a name."
        "%demo_hoist"
    in
    let raise_ignored_error ~loc =
      Location.raise_errorf
        ~loc
        "This let%s expression's pattern is [_]. Please add a name attr or give the \
         variable a name."
        "%demo_hoist"
    in
    let raise_no_binding_error ~loc =
      Location.raise_errorf
        ~loc
        "let%s expressions must be bound to a variable"
        "%demo_hoist"
    in
    fun { pvb_pat; pvb_loc; pvb_attributes; _ } : Name.t ->
      match pvb_pat.ppat_desc with
      | Ppat_var { txt = var_name; loc = ppat_loc; _ }
      | Ppat_constraint
          ({ ppat_desc = Ppat_var { txt = var_name; loc = ppat_loc; _ }; _ }, _) ->
        let original_name = Name.Original.of_string ~loc:ppat_loc var_name in
        let hoisted_name = get_name_from_attributes ~loc:pvb_loc pvb_attributes in
        Name.Original_and_hoisted (original_name, hoisted_name)
      | Ppat_any | Ppat_constraint ({ ppat_desc = Ppat_any; _ }, _) ->
        let hoisted_name = get_name_from_attributes ~loc:pvb_loc pvb_attributes in
        (match hoisted_name with
         | None -> raise_ignored_error ~loc:pvb_loc
         | Some hoisted_name -> Name.No_original hoisted_name)
      | Ppat_construct ({ txt = Lident "()"; _ }, _) ->
        let hoisted_name = get_name_from_attributes ~loc:pvb_loc pvb_attributes in
        (match hoisted_name with
         | None -> raise_unit_error ~loc:pvb_loc
         | Some hoisted_name -> Name.No_original hoisted_name)
      | _ -> raise_no_binding_error ~loc:pvb_loc
  ;;

  (* We shouldn't allow for extracting the code from function expressions. This is because
     the text that is extracted is a bit unexpected

     Ex:

     ```ocaml
     let%demo_hoist foo a b = a + b

     (* Produces this *)

     module Ppx_demo_hoist = struct
       let foo = "a b = a + b"
     end
     ```

     Even if the user were to wrie this as [let%demo_hoist foo = fun a b -> a + b], [apply-style]
     would correct the code to the version in the example above. Users should encapsulate the
     function they want to extract in a module binding if necessary
  *)
  let ensure_expression_is_not_function { pexp_desc; pexp_loc; _ } =
    match pexp_desc with
    | Pexp_function _ ->
      Location.raise_errorf
        ~loc:pexp_loc
        "let%s cannot be used with functions. If you want to extract the source code of \
         a function, please wrap it in a module."
        "%demo_hoist"
    | _ -> ()
  ;;

  let register_let_binding_demo_strings ~loc ~mutable_flag ~rec_flag value_bindings =
    match mutable_flag, rec_flag, value_bindings with
    | Immutable, Nonrecursive, value_bindings ->
      List.iter value_bindings ~f:(fun value_binding ->
        ensure_expression_is_not_function value_binding.pvb_expr;
        let names = get_names_from_value_binding value_binding in
        let loc = ghost_loc value_binding.pvb_loc in
        let demo_string =
          (* We want to retrieve the expression text, not the entire value binding *)
          let loc = value_binding.pvb_expr.pexp_loc in
          create_demo_string ~loc
        in
        Hoister.register_demo_string ~names ~loc demo_string)
    | _, Recursive, _ ->
      Location.raise_errorf
        ~loc
        "let%s cannot be used on a recursive let binding."
        "%demo_hoist"
    | Mutable, _, _ ->
      Location.raise_errorf
        ~loc
        "let%s cannot be used on a mutable let binding."
        "%demo_hoist"
  ;;

  let get_string_from_module_expr ~loc module_expr =
    match module_expr.pmod_desc with
    | Pmod_structure [] -> ""
    | Pmod_structure (first :: tail) ->
      let last = List.last tail |> Option.value ~default:first in
      let loc =
        { loc_start = first.pstr_loc.loc_start
        ; loc_end = last.pstr_loc.loc_end
        ; loc_ghost = true
        }
      in
      create_demo_string ~loc
    | Pmod_extension (_ext_name, _payload) -> create_demo_string ~loc:module_expr.pmod_loc
    | _ ->
      Location.raise_errorf
        ~loc
        "module%s must be bound to a [struct ... end] expression"
        "%demo_string"
  ;;

  let expand : type a. loc:location -> ctxt:Expansion_context.Extension.t -> a t -> a =
    fun ~loc ~ctxt -> function
    | Expression expression ->
      let expression = { expression with pexp_loc = ghost_loc expression.pexp_loc } in
      let ( (* Matching here instead of in the branches so we don't have to rewrite the
               exception raising code *) )
        =
        ensure_expression_is_not_function expression
      in
      (match
         Ppxlib_jane.Shim.Expression_desc.of_parsetree
           ~loc:expression.pexp_loc
           expression.pexp_desc
       with
       | Pexp_let
           ( mutable_flag
           , rec_flag
           , value_bindings
           , _expr
             (* The inner expr is ignored as it has no bearing on how this is parsed *) )
         ->
         (* Hoist each individual value binding. *)
         register_let_binding_demo_strings ~loc ~mutable_flag ~rec_flag value_bindings;
         expression
       | Pexp_letmodule (module_name, module_expr, _expr) ->
         let hoisted_name = get_name_from_attributes ~loc expression.pexp_attributes in
         let name =
           match module_name.txt, hoisted_name with
           | None, None ->
             Location.raise_errorf
               ~loc
               "let%s module expressions must either specify a module name or use the [ \
                [@name ] attribute"
               "%demo_hoist"
           | Some original_name, hoisted_name ->
             Name.Original_and_hoisted
               (Name.Original.of_string ~loc original_name, hoisted_name)
           | None, Some hoisted_name -> Name.No_original hoisted_name
         in
         let string = get_string_from_module_expr ~loc module_expr in
         let () = Hoister.register_demo_string ~names:name ~loc string in
         expression
       | _ ->
         (* This is a fallback to the [Unhoisted] version and acts as a general
            fallthrough *)
         let code_path = Expansion_context.Extension.code_path ctxt in
         let hoisted_name = get_name_from_attributes ~loc expression.pexp_attributes in
         let names =
           match Code_path.enclosing_value code_path, hoisted_name with
           | Some var_name, hoisted_name ->
             let original_name = Name.Original.of_string ~loc var_name in
             Name.Original_and_hoisted (original_name, hoisted_name)
           | None, Some hoisted_name -> Name.No_original hoisted_name
           | None, None ->
             Location.raise_errorf
               ~loc
               "[%s must be bound to a named variable (e.g. let foo = [%s ... ])."
               "%demo_hoist"
               "%demo_hoist"
         in
         let string =
           create_demo_string ~loc
           |> extract_extension_content ~loc ~kind:`Hoisted
           |> Dedent.string
         in
         let () = Hoister.register_demo_string ~names ~loc string in
         expression)
    | Structure_item structure_item ->
      (match structure_item with
       | { pstr_desc = Pstr_module { pmb_name; pmb_attributes = attributes; pmb_expr; _ }
         ; _
         } ->
         let hoisted_name = get_name_from_attributes ~loc attributes in
         let names =
           match pmb_name.txt, hoisted_name with
           | None, None ->
             Location.raise_errorf
               ~loc
               "Modules using %s must be bound to a name (e.g. module%s Foo = ...) or \
                provide a name attribute (e.g. [@name \"Foo\"])."
               "%demo_hoist"
               "%demo_hoist"
           | Some original_name, hoisted_name ->
             Name.Original_and_hoisted
               (Name.Original.of_string ~loc original_name, hoisted_name)
           | None, Some hoisted_name -> Name.No_original hoisted_name
         in
         let string = get_string_from_module_expr ~loc pmb_expr in
         let () = Hoister.register_demo_string ~names ~loc string in
         structure_item
       | { pstr_desc = Pstr_value (rec_flag, value_bindings); pstr_loc; _ } ->
         register_let_binding_demo_strings
           ~loc:pstr_loc
           ~mutable_flag:Immutable
           ~rec_flag
           value_bindings;
         structure_item
       | { pstr_desc = Pstr_recmodule _; _ } ->
         Location.raise_errorf
           ~loc
           "%s cannot be used with recursive modules"
           "%demo_hoist"
       | _ ->
         Location.raise_errorf
           ~loc
           "Only module%s items are allowed in this position"
           "%demo_hoist")
  ;;

  let extensions =
    [ Extension.V3.declare
        hoisted_ppx_name
        Expression
        Ast_pattern.(single_expr_payload __)
        (fun ~ctxt expr ->
          let loc = ghosted_extension_loc ctxt in
          expand ~ctxt ~loc (Expression expr))
    ; Extension.V3.declare
        hoisted_ppx_name
        Structure_item
        Ast_pattern.(pstr (__ ^:: nil))
        (fun ~ctxt item ->
          let loc = ghosted_extension_loc ctxt in
          expand ~ctxt ~loc (Structure_item item))
    ; Extension.V3.declare
        hoisted_ppx_name
        Module_expr
        Ast_pattern.(pstr __)
        (fun ~ctxt _structure ->
          (* Sadly this syntax isn't supported due to the fact that module expressions do
             not receive the attribute that is attached to the [Pstr_module] node that wraps the
             [Pmod_extension] node. Because of this, it's not really possible to ensure that
             the module expression is bound to a name or has a name attribute without some
             weird regex matching *)
          let loc = ghosted_extension_loc ctxt in
          Location.raise_errorf
            ~loc
            "Module expression syntax is not supported by demo_hoist. Please use the \
             [module%s Foo = struct ... end] syntax."
            "%demo_hoist")
    ]
  ;;
end

let () =
  let register_hoister =
    Driver.Instrument.make ~position:Driver.Instrument.After (fun structure ->
      let loc_here = { loc_start = [%here]; loc_end = [%here]; loc_ghost = true } in
      match Hoister.is_empty (), structure with
      | false, [] ->
        Location.raise_errorf
          ~loc:loc_here
          "BUG in ppx_demo, somehow generated side effects from empty file"
      | true, _ -> structure
      | false, first :: _ ->
        let hoister_structure_item =
          Hoister.create_hoisted_module ~loc:(ghost_loc first.pstr_loc)
        in
        Ppx_module_timer_helpers
        .attempt_to_put_structure_item_after_the_ppx_module_timer_start
          ~structure_item:hoister_structure_item
          structure)
  in
  let extensions = Demo.extensions @ Demo_hoist.extensions in
  Driver.register_transformation
    ppx_name
    ~instrument:register_hoister
    ~rules:(List.map extensions ~f:Context_free.Rule.extension)
;;
