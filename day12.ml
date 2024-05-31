open Algo

let has_red = List.exists ~f:(function _, `String "red" -> true | _ -> false)

let rec json_sum ~ignore_red = function
  | `Bool _ -> 0
  | `Assoc kvs ->
      if ignore_red && has_red kvs then 0
      else List.map kvs ~f:(fun (_, v) -> json_sum v ~ignore_red) |> sum
  | `Int n -> n
  | `List l -> List.map l ~f:(json_sum ~ignore_red) |> sum
  | `Null -> 0
  | `String _ -> 0
  | `Float _ -> assert false
  | `Intlit _ -> assert false
  | `Tuple _ -> assert false
  | `Variant _ -> assert false

let%expect_test "part 1" =
  Yojson.Safe.from_string Day12_input.data
  |> json_sum ~ignore_red:false |> printf "%d";
  [%expect {| 191164 |}]

let%expect_test "part 2" =
  Yojson.Safe.from_string Day12_input.data
  |> json_sum ~ignore_red:true |> printf "%d";
  [%expect {| 87842 |}]
