open Core
open Ppxlib

(** This file contains some simple tests to ensure that AST output assumptions that we
    made while writing the PPXes are true *)

let ast_traverse_no_loc =
  object (self)
    inherit Ast_traverse.sexp_of
    method! location _ = self#string "[loc_erased]"
  end
;;

let sexp_of_ast string =
  Lexing.from_string string |> Parse.implementation |> ast_traverse_no_loc#structure
;;

let are_equivalent a b =
  let sexp_a = sexp_of_ast a in
  let sexp_b = sexp_of_ast b in
  Expect_test_helpers_core.require_equal (module Sexp) sexp_a sexp_b
;;

let%expect_test "[%%demo_hoist and module%demo_hoist produce equivalent printed ASTs" =
  are_equivalent
    "[%%demo_hoist module Foo = struct end]"
    "module%demo_hoist Foo = struct end";
  [%expect {| |}];
  are_equivalent
    "[%%demo_hoist module[@name \"hi\"] Foo = struct end]"
    "module%demo_hoist[@name \"hi\"] Foo = struct end";
  [%expect {| |}]
;;

let%expect_test "[%demo_hoist and let%demo_hoist produce equivalent printed ASTs" =
  are_equivalent
    "let () = [%demo_hoist let this_part = () in ()]"
    "let () = let%demo_hoist this_part = () in ()";
  [%expect {| |}]
;;

let%expect_test "[%%demo_hoist and let%demo_hoist at top level produce equivalent \
                 printed ASTs"
  =
  are_equivalent "[%%demo_hoist let this_part = ()]" "let%demo_hoist this_part = ()";
  [%expect {| |}]
;;

let%expect_test "foo a b is equivalent to let foo = fun a b ->" =
  are_equivalent
    "let%demo_hoist foo a b = a + b;;"
    "let%demo_hoist foo = fun a b -> a + b;;";
  [%expect {| |}]
;;
