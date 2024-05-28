open Base
open Stdio

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

let run_p1 () = printf "%d\n" (f1 Input.data)
let run_p2 () = printf "%d\n" (f2 Input.data)

type part = One | Two

let run_part p =
  let r = match p with One -> f1 Input.data | Two -> f2 Input.data in
  printf "%d\n" r

let info = Cmdliner.Cmd.info "day01"

let term =
  let part =
    let open Cmdliner.Arg in
    required
    & opt (some (enum [ ("1", One); ("2", Two) ])) None
    & info [ "part" ]
  in
  Cmdliner.Term.(const run_part $ part)

let cmd = Cmdliner.Cmd.v info term
let run () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
