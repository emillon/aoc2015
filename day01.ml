let f a c =
  match c with
  | '(' -> a + 1
  | ')' -> a - 1
  | '\n' -> a
  | _ -> raise_s [%message (c : char)]

let f1 s = String.fold s ~init:0 ~f

let%expect_test "f1" =
  let test input = print_s ([%sexp_of: int] (f1 input)) in
  test "(())";
  [%expect {| 0 |}];
  test "()()";
  [%expect {| 0 |}];
  test "(((";
  [%expect {| 3 |}];
  test "(()(()(";
  [%expect {| 3 |}];
  test "))(((((";
  [%expect {| 3 |}];
  test "())";
  [%expect {| -1 |}];
  test "))(";
  [%expect {| -1 |}];
  test ")))";
  [%expect {| -3 |}];
  test ")())())";
  [%expect {| -3 |}]

let f2 s =
  String.foldi s ~init:(Ok 0) ~f:(fun i state c ->
      let%bind.Result a = state in
      match f a c with -1 -> Error (i + 1) | a' -> Ok a')
  |> Result.error |> Option.value_exn

let%expect_test "f2" =
  let test input = print_s ([%sexp_of: int] (f2 input)) in
  test ")";
  [%expect {| 1 |}];
  test "()())";
  [%expect {| 5 |}]

let run () = Run.run ~name:"day01" ~f1 ~f2 Day01_input.data
