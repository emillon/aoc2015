open Algo
open Parsing_util

type ingredient = {
  capacity : int;
  durability : int;
  flavor : int;
  texture : int;
  calories : int;
}
[@@deriving sexp]

let clamp n = Int.max 0 n

let ingredient_score { capacity; durability; flavor; texture; calories = _ } =
  List.map ~f:clamp [ capacity; durability; flavor; texture ] |> product

let parse_line =
  let signed_number =
    let open Angstrom in
    let+ minus = option false (char '-' *> return true) and+ number = number in
    if minus then -number else number
  in
  parse_using
  @@
  let open Angstrom in
  let+ name = word <* string ": "
  and+ capacity = string "capacity " *> signed_number <* string ", "
  and+ durability = string "durability " *> signed_number <* string ", "
  and+ flavor = string "flavor " *> signed_number <* string ", "
  and+ texture = string "texture " *> signed_number <* string ", "
  and+ calories = string "calories " *> signed_number in
  (name, { capacity; durability; flavor; texture; calories })

let parse s =
  String.split_lines s |> List.map ~f:parse_line
  |> Map.of_alist_exn (module String)

let sample =
  String.concat_lines
    [
      "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories \
       8";
      "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3";
    ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: ingredient Map.M(String).t] |> print_s;
  [%expect
    {|
    ((Butterscotch
      ((capacity -1) (durability -2) (flavor 6) (texture 3) (calories 8)))
     (Cinnamon
      ((capacity 2) (durability 3) (flavor -2) (texture -1) (calories 3)))) |}]

let add t { capacity; durability; flavor; texture; calories } =
  {
    capacity = capacity + t.capacity;
    durability = durability + t.durability;
    flavor = flavor + t.flavor;
    texture = texture + t.texture;
    calories = calories + t.calories;
  }

let smult t n =
  {
    capacity = t.capacity * n;
    durability = t.durability * n;
    flavor = t.flavor * n;
    texture = t.texture * n;
    calories = t.calories * n;
  }

let zero =
  { capacity = 0; durability = 0; flavor = 0; texture = 0; calories = 0 }

let mult m p =
  Map.fold p ~init:zero ~f:(fun ~key ~data acc ->
      add acc (smult (Map.find_exn m key) data))

let score_for_props m props = mult m props |> ingredient_score

let%expect_test "score_for_props" =
  let m = parse sample in
  let props =
    Map.of_alist_exn (module String) [ ("Butterscotch", 44); ("Cinnamon", 56) ]
  in
  score_for_props m props |> printf "%d";
  [%expect {| 62842880 |}]

let int_partitions ~max ~n =
  let rec go ~max ~n ~left =
    if left <= 0 then []
    else if n = 1 then [ [ left ] ]
    else
      let open List.Let_syntax in
      let%bind i = List.range 1 max in
      let%map p = go ~max ~n:(n - 1) ~left:(left - i) in
      i :: p
  in
  go ~max ~n ~left:max

let%expect_test "int_partitions" =
  let test ~max ~n =
    int_partitions ~max ~n |> [%sexp_of: int list list] |> print_s
  in
  test ~max:2 ~n:2;
  [%expect {| ((1 1)) |}];
  test ~max:3 ~n:2;
  [%expect {| ((1 2) (2 1)) |}];
  test ~max:10 ~n:3;
  [%expect
    {|
    ((1 1 8) (1 2 7) (1 3 6) (1 4 5) (1 5 4) (1 6 3) (1 7 2) (1 8 1) (2 1 7)
     (2 2 6) (2 3 5) (2 4 4) (2 5 3) (2 6 2) (2 7 1) (3 1 6) (3 2 5) (3 3 4)
     (3 4 3) (3 5 2) (3 6 1) (4 1 5) (4 2 4) (4 3 3) (4 4 2) (4 5 1) (5 1 4)
     (5 2 3) (5 3 2) (5 4 1) (6 1 3) (6 2 2) (6 3 1) (7 1 2) (7 2 1) (8 1 1)) |}]

let all_props keys =
  let n = List.length keys in
  let partitions = int_partitions ~max:100 ~n in
  List.map partitions ~f:(fun partition ->
      List.zip_exn keys partition |> Map.of_alist_exn (module String))

let f_gen ~f s =
  let m = parse s in
  let keys = Map.keys m in
  let ap = all_props keys in
  List.filter_map ap ~f:(f m)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let f1 = f_gen ~f:(fun m props -> Some (score_for_props m props))

let f2 =
  f_gen ~f:(fun m props ->
      let r = mult m props in
      if r.calories = 500 then Some (ingredient_score r) else None)

let run () = Run.run ~name:"day15" ~f1 ~f2 Day15_input.data
