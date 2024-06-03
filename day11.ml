open Algo

let is_increasing a b c =
  let na = Char.to_int a in
  let nb = Char.to_int b in
  let nc = Char.to_int c in
  na + 1 = nb && nb + 1 = nc

let req1 s =
  let s_len = String.length s in
  String.existsi s ~f:(fun i c1 ->
      i + 2 < s_len
      &&
      let c2 = String.get s (i + 1) in
      let c3 = String.get s (i + 2) in
      is_increasing c1 c2 c3)

let req2 s =
  String.for_all s ~f:(function 'i' | 'o' | 'l' -> false | _ -> true)

let pair_after ?excluding s i_start =
  let s_len = String.length s in
  let rec go i =
    if i >= s_len - 1 then None
    else
      let ca = String.get s i in
      let cb = String.get s (i + 1) in
      let equal = Char.equal ca cb in
      let ok =
        match excluding with
        | None -> equal
        | Some ce -> equal && not (Char.equal ca ce)
      in
      if ok then Some (i, ca) else go (i + 1)
  in
  go i_start

let req3 s =
  Option.is_some
    (let%bind.Option first_index, first_char = pair_after s 0 in
     pair_after s (first_index + 2) ~excluding:first_char)

let ok s = req1 s && req2 s && req3 s

let%expect_test "ok" =
  let test s = ok s |> printf "%b" in
  test "hijklmmn";
  [%expect {| false |}];
  test "abbceffg";
  [%expect {| false |}];
  test "abbcegjk";
  [%expect {| false |}]

let iter f s =
  iter_bytes ~min:'a' ~max:'z' ~start:s ~f:(fun b ->
      f (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b))

let next s = iter ok s

let%expect_test "next" =
  let test s = next s |> printf "%s" in
  test "abcdefgh";
  [%expect {| abcdffaa |}];
  test "ghijklmn";
  [%expect {| ghjaabcc |}]

let f1 = next

let f2 s =
  let p1 = next s in
  iter (fun s -> ok s && not (String.equal s p1)) p1

let run () = Run.run_string ~name:"day11" ~f1 ~f2 "vzbxkghb"
