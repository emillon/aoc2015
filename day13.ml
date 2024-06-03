open Algo
open Parsing_util

module Key = struct
  module T = struct
    type t = string * string [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let parse_line =
  let sign =
    let open Angstrom in
    choice [ string "gain" *> return Fn.id; string "lose" *> return Int.neg ]
  in
  parse_using
  @@
  let open Angstrom in
  let+ who = word <* string " would "
  and+ sign = sign <* string " "
  and+ n = number <* string " happiness units by sitting next to "
  and+ neighbour = word <* string "." in
  (who, neighbour, sign n)

let edges l =
  let wrapped = List.last_exn l :: l in
  legs wrapped |> List.concat_map ~f:(fun (x, y) -> [ (x, y); (y, x) ])

let build_problem s =
  String.split_lines s |> List.map ~f:parse_line
  |> List.fold
       ~init:(Map.empty (module Key), Set.empty (module String))
       ~f:(fun (map, set) (who, neighbour, diff) ->
         (Map.add_exn map ~key:(who, neighbour) ~data:diff, Set.add set who))

let solve_problem (map, people) =
  let edge_diff k = Map.find_exn map k in
  people |> Set.to_list |> permutations
  |> List.map ~f:(fun l -> edges l |> List.map ~f:edge_diff |> sum)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let f1 s = build_problem s |> solve_problem

let sample =
  String.concat_lines
    [
      "Alice would gain 54 happiness units by sitting next to Bob.";
      "Alice would lose 79 happiness units by sitting next to Carol.";
      "Alice would lose 2 happiness units by sitting next to David.";
      "Bob would gain 83 happiness units by sitting next to Alice.";
      "Bob would lose 7 happiness units by sitting next to Carol.";
      "Bob would lose 63 happiness units by sitting next to David.";
      "Carol would lose 62 happiness units by sitting next to Alice.";
      "Carol would gain 60 happiness units by sitting next to Bob.";
      "Carol would gain 55 happiness units by sitting next to David.";
      "David would gain 46 happiness units by sitting next to Alice.";
      "David would lose 7 happiness units by sitting next to Bob.";
      "David would gain 41 happiness units by sitting next to Carol.";
    ]

let%expect_test "f1" =
  f1 sample |> printf "%d";
  [%expect {| 330 |}]

let add_self (map, people) =
  let me = "@" in
  let new_map =
    Set.fold people ~init:map ~f:(fun acc other ->
        acc
        |> Map.add_exn ~key:(me, other) ~data:0
        |> Map.add_exn ~key:(other, me) ~data:0)
  in
  let new_people = Set.add people me in
  (new_map, new_people)

let f2 s = build_problem s |> add_self |> solve_problem
let run () = Run.run ~name:"day13" ~f1 ~f2 Day13_input.data
