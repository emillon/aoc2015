let is_vowel = function 'a' | 'e' | 'i' | 'o' | 'u' -> true | _ -> false

let has_repeated_letter s =
  let s_len = String.length s in
  String.existsi s ~f:(fun i c1 ->
      i + 1 < s_len
      &&
      let c2 = String.get s (i + 1) in
      Char.equal c1 c2)

let has_three_vowels s =
  String.to_list s |> List.filter ~f:is_vowel |> List.length >= 3

let contains_bad =
  let bad =
    List.map ~f:String.Search_pattern.create [ "ab"; "cd"; "pq"; "xy" ]
  in
  fun s -> List.exists bad ~f:(fun pat -> String.Search_pattern.matches pat s)

let is_nice s =
  has_three_vowels s && has_repeated_letter s && not (contains_bad s)

let%expect_test "is_nice" =
  let test s = is_nice s |> printf "%b" in
  test "ugknbfddgicrmopn";
  [%expect {| true |}];
  test "aaa";
  [%expect {| true |}];
  test "jchzalrnumimnmhp";
  [%expect {| false |}];
  test "haegwjzuvuyypxyu";
  [%expect {| false |}];
  test "dvszwmarrgswjxmb";
  [%expect {| false |}]

let has_xyxy =
  let re = Str.regexp {|.*\(..\).*\1.*|} in
  fun s -> Str.string_match re s 0

let has_xyx s =
  let s_len = String.length s in
  String.existsi s ~f:(fun i c1 ->
      i + 2 < s_len
      &&
      let c3 = String.get s (i + 2) in
      Char.equal c1 c3)

let is_nice2 s = has_xyxy s && has_xyx s

let%expect_test "is_nice2" =
  let test s = is_nice2 s |> printf "%b" in
  test "qjhvhtzxzqqjkmpb";
  [%expect {| true |}];
  test "xxyxx";
  [%expect {| true |}];
  test "uurcxstgmygtbstg";
  [%expect {| false |}];
  test "ieodomkazucvgmuy";
  [%expect {| false |}]

let f1 s = String.split_lines s |> List.count ~f:is_nice
let f2 s = String.split_lines s |> List.count ~f:is_nice2
let run () = Run.run ~name:"day05" ~f1 ~f2 Day05_input.data
