open Parsing_util

let pos (a, b) = ((a + b) * (a + b - 1) / 2) - b

let%expect_test "pos" =
  let test (a, b) = pos (a, b) |> printf "%d" in
  test (1, 1);
  [%expect {| 0 |}];
  test (1, 2);
  [%expect {| 1 |}];
  test (2, 1);
  [%expect {| 2 |}];
  test (1, 3);
  [%expect {| 3 |}];
  test (2, 2);
  [%expect {| 4 |}];
  test (3, 1);
  [%expect {| 5 |}];
  test (1, 4);
  [%expect {| 6 |}];
  test (2, 3);
  [%expect {| 7 |}];
  test (3, 2);
  [%expect {| 8 |}];
  test (4, 1);
  [%expect {| 9 |}]

let f n = n * 252533 % 33554393

let parse =
  parse_using
  @@
  let open Angstrom in
  let+ row =
    string
      "To continue, please consult the code grid in the manual.  Enter the \
       code at row "
    *> number
  and+ column = string ", column " *> number <* string "." <* end_of_line in
  (column, row)

let f1 s =
  let n = parse s |> pos in
  Fn.apply_n_times ~n f 2015_11_25

let f2 _ = 0
let run () = Run.run ~name:"day25" ~f1 ~f2 Day25_input.data
