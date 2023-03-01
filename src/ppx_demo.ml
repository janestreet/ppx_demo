open! Core
open Ppxlib
open Ast_builder.Default

let ppx_name = "demo"
let files = String.Table.create ()

let read_file filename =
  match Hashtbl.find files filename with
  | Some file_contents -> file_contents
  | None ->
    let file_contents = In_channel.read_all filename in
    Hashtbl.add_exn files ~key:filename ~data:file_contents;
    file_contents
;;

let create_demo_string ~loc_start ~loc_end =
  (* We start the substring a little further into the string so that we can
     strip off the leading "[%demo" of the expression. *)
  let start_index = loc_start.pos_cnum + String.length ppx_name + 2 in
  (* We end the substring one character earlier to strip off the trailing "]"
     from the ppx. *)
  let end_index = loc_end.pos_cnum - 1 in
  let file_contents = read_file loc_start.pos_fname in
  let () =
    let sanity_check_substring =
      String.sub file_contents ~pos:loc_start.pos_cnum ~len:(String.length ppx_name + 2)
    in
    if not (String.equal sanity_check_substring "[%demo")
    then failwith "ppx_demo requires that extension node be of the form [%demo ...]"
  in
  let substring_length = end_index - start_index in
  let pad_length = start_index - loc_start.pos_bol in
  let buffer = Buffer.create (substring_length + pad_length) in
  for _ = 0 to pad_length - 1 do
    Buffer.add_char buffer ' '
  done;
  Buffer.add_substring buffer file_contents ~pos:start_index ~len:substring_length;
  Buffer.contents buffer |> Dedent.string
;;

let expand_expr ~loc:{ loc_start; loc_end; _ } ~path:_ expr =
  let string = create_demo_string ~loc_start ~loc_end in
  let loc = { loc_start; loc_end; loc_ghost = true } in
  pexp_tuple ~loc [ expr; estring ~loc string ]
;;

let expand_str ~loc:{ loc_start; loc_end; _ } ~path:_ (structure : structure) =
  let string = create_demo_string ~loc_start ~loc_end in
  let loc = { loc_start; loc_end; loc_ghost = true } in
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

let () =
  Driver.register_transformation
    ppx_name
    ~extensions:
      [ Extension.declare
          ppx_name
          Extension.Context.expression
          Ast_pattern.(single_expr_payload __)
          expand_expr
      ; Extension.declare
          ppx_name
          Extension.Context.module_expr
          Ast_pattern.(pstr __)
          expand_str
      ]
;;
