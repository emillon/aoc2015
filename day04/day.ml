open Base
open Stdio

let count_leading_zeroes s =
  String.fold_until s ~init:0
    ~f:(fun i c -> match c with '0' -> Continue (i + 1) | _ -> Stop i)
    ~finish:Fn.id

let score s i =
  Printf.sprintf "%s%d" s i |> Stdlib.Digest.string |> Stdlib.Digest.to_hex
  |> count_leading_zeroes

let f_gen s n =
  let rec go i = if score s i >= n then i else go (i + 1) in
  go 0

let f1 s = f_gen s 5

let%expect_test "f1" =
  let test s = f1 s |> printf "%d" in
  test "abcdef";
  [%expect {| 609043 |}];
  test "pqrstuv";
  [%expect {| 1048970 |}]

let f2 s = f_gen s 6
let input_data = "iwrupvqb"
let run () = Lib.run ~name:"day03" ~f1 ~f2 input_data
