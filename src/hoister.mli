open! Core
open Ppxlib

val register_demo_string : 'a. loc:location -> names:Name.t -> string -> unit
val create_hoisted_module : loc:location -> structure_item
val is_empty : unit -> bool
