open Algo

let bytes_zeroes = function
  | '\x00' -> 2
  | '\x01' .. '\x0f' -> 1
  | '\x10' .. '\xff' -> 0

let count_leading_hex_zeroes s =
  let s_len = String.length s in
  let rec go i r =
    if i >= s_len then r
    else
      let z = bytes_zeroes (String.unsafe_get s i) in
      let r' = r + z in
      if z = 2 then go (i + 1) r' else r'
  in
  go 0 0

let%expect_test "count_leading_hex_zeroes" =
  let test c = count_leading_hex_zeroes c |> printf "%d" in
  test "\x00\x00\xfd";
  [%expect {| 4 |}];
  test "\x00\x00\x0d";
  [%expect {| 5 |}];
  test "\xab\x00\x0d";
  [%expect {| 0 |}]

let score d_s i_b =
  Digestif.MD5.feed_bytes d_s i_b
  |> Digestif.MD5.get |> Digestif.MD5.to_raw_string |> count_leading_hex_zeroes

let f_gen s n =
  let d_s = Digestif.MD5.feed_string Digestif.MD5.empty s in
  iter_bytes ~start:"0" ~min:'0' ~max:'9' ~f:(fun b -> score d_s b >= n)
  |> Int.of_string

let f1 s = f_gen s 5

let%expect_test "f1" =
  let test s = f1 s |> printf "%d" in
  test "abcdef";
  [%expect {| 609043 |}];
  test "pqrstuv";
  [%expect {| 1048970 |}]

let input_data = "iwrupvqb"

let%expect_test "part 1" =
  f1 input_data |> printf "%d";
  [%expect {| 346386 |}]

let%expect_test "part 2" =
  f_gen input_data 6 |> printf "%d";
  [%expect {| 9958218 |}]
