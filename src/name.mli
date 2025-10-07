open! Core

val ocaml_identifier_regex_string : string
val ocaml_identifier_regex : Re2.t

(** Using very simple newtypes here so that we can have a bit of type safety when putting
    the names in one position or another. Also helps prevent accidentally putting a name
    where a demo string should go *)
module Hoisted : sig
  type t [@@deriving sexp, equal, compare, hash]

  val of_string : loc:Ppxlib.location -> string -> t
  val to_string : t -> string
  val is_capitalized : t -> bool
end

module Original : sig
  type t [@@deriving sexp, equal, compare, hash]

  val of_string : loc:Ppxlib.location -> string -> t
  val to_hoisted : t -> Hoisted.t
  val to_string : t -> string
end

type t =
  | No_original of Hoisted.t
  | Original_and_hoisted of Original.t * Hoisted.t option
