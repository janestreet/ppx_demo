open! Core

let%expect_test "test expression bindings" =
  print_endline Ppx_demo_hoist.uh;
  [%expect {| "hello" |}];
  (* Make sure we are able to print these values before they're defined due to hoisting *)
  print_endline Ppx_demo_hoist.result;
  [%expect {| 1 + 2 + 3 |}];
  let result = [%demo_hoist 1 + 2 + 3] in
  print_s [%sexp (result : int)];
  [%expect {| 6 |}];
  print_endline Ppx_demo_hoist.bar;
  [%expect {| "i am bar" |}];
  let bar = [%demo_hoist "i am bar"] in
  print_s [%sexp (bar : string)];
  [%expect {| "i am bar" |}];
  let%demo_hoist uh = "hello" in
  print_s [%message (uh : string)];
  [%expect {| (uh hello) |}];
  let%demo_hoist[@name "and_1"] (_ : string) = "ignored and_1"
  and[@name "and_2"] () = () in
  print_endline Ppx_demo_hoist.and_1;
  [%expect {| "ignored and_1" |}];
  print_endline Ppx_demo_hoist.and_2;
  [%expect {| () |}]
;;

let%expect_test "test top-level value bindings" =
  print_endline Ppx_demo_hoist.top_level_a;
  [%expect {| "hi" |}];
  print_endline Ppx_demo_hoist.renamed_from_b;
  [%expect {| "renamed" |}]
;;

let%demo_hoist top_level_a = "hi"

let%demo_hoist[@name "renamed_from_b"] top_level_b = "renamed"
and[@name "renamed_from_c"] top_level_c = "also renamed"

let%expect_test "test top-level value bindings continued" =
  print_endline top_level_a;
  [%expect {| hi |}];
  print_endline Ppx_demo_hoist.top_level_a;
  [%expect {| "hi" |}];
  print_endline top_level_b;
  [%expect {| renamed |}];
  print_endline top_level_c;
  [%expect {| also renamed |}];
  print_endline Ppx_demo_hoist.renamed_from_c;
  [%expect {| "also renamed" |}];
  print_endline Ppx_demo_hoist.renamed_from_b;
  [%expect {| "renamed" |}]
;;

let%expect_test "module hoisting" =
  print_endline Ppx_demo_hoist.TestModule.ppx_demo_string;
  [%expect {| let x = 42 |}];
  print_endline Ppx_demo_hoist.Bar.ppx_demo_string;
  [%expect {| let test = "hi" |}];
  print_endline Ppx_demo_hoist.Ignored.ppx_demo_string;
  [%expect
    {|
    let () = ()
    let i_am_ignored = "yep"
    let () = ignore i_am_ignored
    |}];
  print_endline Ppx_demo_hoist.Uh_oh.ppx_demo_string;
  [%expect {| let a = "uh oh" |}];
  print_endline Ppx_demo_hoist.Stri.ppx_demo_string;
  [%expect
    {|
    let () = ()
    let another = "one"
    |}]
;;

module%demo_hoist TestModule = struct
  let x = 42
end

module X = struct
  let test =
    let module F = struct
      module T = struct
        module%demo_hoist [@name "Bar"] Foo = struct
          let test = "hi"
        end
      end
    end
    in
    F.T.Foo.test
  ;;
end

[%%demo_hoist
  module Stri = struct
    let () = ()
    let another = "one"
  end]

[%%demo_hoist
  module [@name "Ignored"] _ = struct
    let () = ()
    let i_am_ignored = "yep"
    let () = ignore i_am_ignored
  end]

let%expect_test "module hoisting continued" =
  let module%demo_hoist Uh_oh = struct
    let a = "uh oh"
  end
  in
  print_s [%message (Uh_oh.a : string)];
  [%expect {| (Uh_oh.a "uh oh") |}];
  print_s [%message (TestModule.x : int)];
  [%expect {| (TestModule.x 42) |}];
  print_s [%message (X.test : string)];
  [%expect {| (X.test hi) |}];
  print_s [%message (Stri.another : string)];
  [%expect {| (Stri.another one) |}]
;;

module%demo_hoist [@name "var_name_module"] _ = struct
  let _x = ()
end

let%expect_test "module hoisted as var" =
  print_endline Ppx_demo_hoist.var_name_module;
  [%expect {| let _x = () |}]
;;

let%expect_test "ignored var hoisted as module" =
  let%demo_hoist[@name "Var_module"] _ = () in
  print_endline Ppx_demo_hoist.Var_module.ppx_demo_string;
  [%expect {| () |}]
;;

let%expect_test "Ignored var works with type constraint" =
  print_endline Ppx_demo_hoist.ignored_with_constraint;
  [%expect {| "ignored with constraint" |}];
  let%demo_hoist[@name "ignored_with_constraint"] (_ : string) =
    "ignored with constraint"
  in
  ()
;;

let%expect_test "Var works with type constraint" =
  print_endline Ppx_demo_hoist.regular_var_with_constraint;
  [%expect {| "I have a string constraint" |}];
  let%demo_hoist[@name "regular_var_with_constraint"] (regular_var : string) =
    "I have a string constraint"
  in
  print_endline regular_var;
  [%expect {| I have a string constraint |}]
;;

let%expect_test "ignored name still works if not [_]" =
  print_endline Ppx_demo_hoist._huh;
  [%expect {| () |}];
  print_endline Ppx_demo_hoist._bar;
  [%expect
    {|
    let () = () in
    ()
    |}];
  let%demo_hoist _huh = () in
  let%demo_hoist[@name "_bar"] _huh =
    let () = () in
    ()
  in
  print_endline Ppx_demo_hoist.ignored_top_level;
  [%expect {| () |}]
;;

(* This one is not an error as it's handled by [Pstr_value] *)
[%%demo_hoist let[@name "ignored_top_level"] _ = ()]

let%expect_test "nested hoists work" =
  print_endline Ppx_demo_hoist.Above.ppx_demo_string;
  [%expect
    {|
    module%demo_hoist Below = struct
      let%demo_hoist nested_value = "Above.Below.nested"
    end
    |}];
  print_endline Ppx_demo_hoist.Below.ppx_demo_string;
  [%expect {| let%demo_hoist nested_value = "Above.Below.nested" |}];
  print_endline Ppx_demo_hoist.nested_value;
  [%expect {| "Above.Below.nested" |}]
;;

module%demo_hoist Above = struct
  module%demo_hoist Below = struct
    let%demo_hoist nested_value = "Above.Below.nested"
  end
end

let%expect_test "nested hoists work continued" =
  print_endline Above.Below.nested_value;
  [%expect {| Above.Below.nested |}]
;;

let%expect_test "test ppx interpolation points" =
  print_endline Ppx_demo_hoist.Test_module_expr.ppx_demo_string;
  [%expect {| [%demo let () = ()] |}];
  print_endline Ppx_demo_hoist.Test_module_expr_with_attr.ppx_demo_string;
  [%expect {| [%demo let () = ()] |}]
;;

module%demo_hoist Test_module_expr = [%demo let () = ()]
module%demo_hoist Test_module_expr_with_attr = [%demo let () = ()] [@ocaml.warning "-60"]

let%expect_test "test ppx interpolation points continued" =
  print_endline Test_module_expr.ppx_demo_string;
  [%expect {| let () = () |}];
  print_endline Test_module_expr_with_attr.ppx_demo_string;
  [%expect {| let () = () |}]
;;
