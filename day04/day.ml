open Base
open Stdio

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

exception Overflow

let rec incr_buf_at_point b i =
  if i < 0 then raise Overflow;
  match Bytes.get b i with
  | '9' ->
      Bytes.set b i '0';
      incr_buf_at_point b (i - 1)
  | '8' -> Bytes.set b i '9'
  | '7' -> Bytes.set b i '8'
  | '6' -> Bytes.set b i '7'
  | '5' -> Bytes.set b i '6'
  | '4' -> Bytes.set b i '5'
  | '3' -> Bytes.set b i '4'
  | '2' -> Bytes.set b i '3'
  | '1' -> Bytes.set b i '2'
  | '0' -> Bytes.set b i '1'
  | _ -> assert false

let incr_buf b = incr_buf_at_point b (Bytes.length b - 1)

let resize_b b =
  let r = Bytes.make (Bytes.length b + 1) '0' in
  Bytes.set r 0 '1';
  r

let f_gen s n =
  let d_s = Digestif.MD5.feed_string Digestif.MD5.empty s in
  let rec go b =
    if score d_s b >= n then Bytes.to_string b |> Int.of_string
    else
      match incr_buf b with () -> go b | exception Overflow -> go (resize_b b)
  in
  go (Bytes.of_string "0")

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
