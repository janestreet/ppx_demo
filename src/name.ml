open! Core

(** Pulled from the following url:
    https://ocaml.org/manual/5.3/lex.html#:~:text=Identifiers%20are%20sequences%20of%20letters%2C%20digits%2C%20_%20(the%20underscore%20character)%2C%20and%20%27%20(the%20single%20quote)%2C%20starting%20with%20a%20letter%20or%20an%20underscore

    We are only allowing ASCII letters *)
let ocaml_identifier_regex_string = {|[A-Za-z_][A-Za-z0-9_']*|}

let ocaml_identifier_regex = Re2.create_exn {%string|^%{ocaml_identifier_regex_string}$|}

let name_of_string ~loc name =
  match Re2.matches ocaml_identifier_regex name with
  | true -> name
  | false ->
    Ppxlib.Location.raise_errorf
      ~loc
      "Ppx_demo_hoist received a name attribute with value '%s', which is not a valid \
       OCaml identifier. Please choose a different name."
      name
;;

module Hoisted = struct
  include
    String_id.Make
      (struct
        let module_name = "Name.Hoisted"
      end)
      ()

  let of_string' = of_string
  let of_string ~loc value = name_of_string ~loc value |> of_string'

  let is_capitalized name =
    let name = to_string name in
    Char.is_uppercase (String.get name 0)
  ;;
end

module Original = struct
  include
    String_id.Make
      (struct
        let module_name = "Name.Original"
      end)
      ()

  let of_string ~loc value = name_of_string ~loc value |> of_string
  let to_hoisted value = to_string value |> Hoisted.of_string'
end

type t =
  | No_original of Hoisted.t
  | Original_and_hoisted of Original.t * Hoisted.t option
