open Base
open Stdio
open Lib

type sizes = { code : int; memory : int } [@@deriving sexp]

let size_diff { code; memory } = code - memory

type state = Start | In_string | Done | Escape | Escape_x1 | Escape_x2
[@@deriving sexp]

let finalize = function
  | Start, _ -> None
  | In_string, _ -> None
  | Done, n -> Some n
  | Escape, _ -> None
  | Escape_x1, _ -> None
  | Escape_x2, _ -> None

let memory_size s =
  String.fold s ~init:(Start, 0) ~f:(fun (state, n) c ->
      match (state, c) with
      | Start, '"' -> (In_string, 0)
      | In_string, '"' -> (Done, n)
      | In_string, 'a' .. 'z' -> (In_string, n + 1)
      | In_string, '\\' -> (Escape, n)
      | Escape, ('"' | '\\') -> (In_string, n + 1)
      | Escape, 'x' -> (Escape_x1, n)
      | Escape_x1, ('0' .. '9' | 'a' .. 'f') -> (Escape_x2, n)
      | Escape_x2, ('0' .. '9' | 'a' .. 'f') -> (In_string, n + 1)
      | _ -> raise_s [%message (state : state) (c : char)])
  |> finalize |> Option.value_exn

let sizes s = { code = String.length s; memory = memory_size s }

let%expect_test "size" =
  let test s = sizes s |> [%sexp_of: sizes] |> print_s in
  test {|""|};
  [%expect {| ((code 2) (memory 0)) |}];
  test {|"abc"|};
  [%expect {| ((code 5) (memory 3)) |}];
  test {|"aaa\"aaa"|};
  [%expect {| ((code 10) (memory 7)) |}];
  test {|"\x27"|};
  [%expect {| ((code 6) (memory 1)) |}]

let global_diff s ~f =
  String.split_lines s |> List.map ~f:(fun s -> f s |> size_diff) |> sum

let f1 = global_diff ~f:sizes

let encoded_char_size = function
  | '"' -> 2
  | '\\' -> 2
  | 'a' .. 'z' | '0' .. '9' -> 1
  | c -> raise_s [%message (c : char)]

let encoded_size s =
  2 + String.fold s ~init:0 ~f:(fun a c -> a + encoded_char_size c)

let after_encoding s = { memory = String.length s; code = encoded_size s }

let%expect_test "encode" =
  let test s = after_encoding s |> [%sexp_of: sizes] |> print_s in
  test {|""|};
  [%expect {| ((code 6) (memory 2)) |}];
  test {|"abc"|};
  [%expect {| ((code 9) (memory 5)) |}];
  test {|"aaa\"aaa"|};
  [%expect {| ((code 16) (memory 10)) |}];
  test {|"\x27"|};
  [%expect {| ((code 11) (memory 6)) |}]

let f2 = global_diff ~f:after_encoding
let run () = Lib.run ~name:"day07" ~f1 ~f2 Input.data
