open! Core

let%expect_test _ =
  let x = 5 in
  let value, string = [%demo x + 10] in
  print_endline string;
  print_endline "==========";
  print_endline (Int.to_string value);
  [%expect {|
    x + 10
    ==========
    15 |}]
;;

module M = struct
  type 'a t = T of 'a

  let of_int (x : int) = T x
  let of_string (x : string) = T x

  module Let_syntax = struct
    module Let_syntax = struct
      let bind (T x) ~f = f x
      let map (T x) ~f = T (f x)
    end
  end
end

let%expect_test _ =
  let T value, string =
    [%demo
      let%bind.M pos =
        let%map.M a = M.of_int 70 in
        a
      in
      let%bind.M long_string =
        M.of_string
          "xxxxx     xxxxx     xxxxx     xxxxx     xxxxx     xxxxx     xxxxx     \
           contents     xxxxx"
      in
      let%map.M len = M.of_int 8 in
      String.sub long_string ~pos ~len |> fun x -> x]
  in
  print_endline string;
  print_endline "==========";
  print_endline value;
  [%expect
    {|
    let%bind.M pos =
      let%map.M a = M.of_int 70 in
      a
    in
    let%bind.M long_string =
      M.of_string
        "xxxxx     xxxxx     xxxxx     xxxxx     xxxxx     xxxxx     xxxxx     \
         contents     xxxxx"
    in
    let%map.M len = M.of_int 8 in
    String.sub long_string ~pos ~len |> fun x -> x
    ==========
    contents |}]
;;

let%expect_test "demoing a module's structure" =
  let module Example =
    [%demo
      let x = 1

      module T = struct
        type t =
          | Foo
          | Bar
        [@@deriving sexp]

        let a = Foo
        let b = Bar
      end]
  in
  let x = Example.x in
  let a = Example.T.a in
  let b = Example.T.b in
  print_s [%message (x : int) (a : Example.T.t) (b : Example.T.t)];
  [%expect {| ((x 1) (a Foo) (b Bar)) |}];
  print_endline Example.ppx_demo_string;
  [%expect
    {|
    let x = 1

    module T = struct
      type t =
        | Foo
        | Bar
      [@@deriving sexp]

      let a = Foo
      let b = Bar
    end |}]
;;
