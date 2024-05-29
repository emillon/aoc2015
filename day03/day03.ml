open Base
open Stdio

module Dir = struct
  type t = N | S | E | W

  let of_char = function
    | '^' -> N
    | 'v' -> S
    | '>' -> E
    | '<' -> W
    | c -> raise_s [%message "Dir.of_char" (c : char)]
end

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let shift (x, y) (d : Dir.t) =
    match d with
    | N -> (x, y - 1)
    | S -> (x, y + 1)
    | E -> (x + 1, y)
    | W -> (x - 1, y)
end

let f1 s =
  let s = String.strip s in
  let origin = (0, 0) in
  let init_visited = Set.singleton (module Pos) origin in
  String.fold s ~init:(origin, init_visited) ~f:(fun (pos, visited) c ->
      let new_pos = Pos.shift pos (Dir.of_char c) in
      let new_visited = Set.add visited new_pos in
      (new_pos, new_visited))
  |> snd |> Set.length

let%expect_test "f1" =
  let test s = f1 s |> printf "%d" in
  test ">";
  [%expect {| 2 |}];
  test "^>v<";
  [%expect {| 4 |}];
  test "^v^v^v^v^v";
  [%expect {| 2 |}]

let f2 s =
  let s = String.strip s in
  let origin = (0, 0) in
  let init_visited = Set.singleton (module Pos) origin in
  let _, _, _, visited =
    String.fold s ~init:(origin, origin, true, init_visited)
      ~f:(fun (santa_pos, robo_pos, santas_turn, visited) c ->
        let dir = Dir.of_char c in
        let new_santa_pos, new_robo_pos, new_pos =
          if santas_turn then
            let new_pos = Pos.shift santa_pos dir in
            (new_pos, robo_pos, new_pos)
          else
            let new_pos = Pos.shift robo_pos dir in
            (santa_pos, new_pos, new_pos)
        in
        let new_santas_turn = not santas_turn in
        let new_visited = Set.add visited new_pos in
        (new_santa_pos, new_robo_pos, new_santas_turn, new_visited))
  in
  Set.length visited

let%expect_test "f2" =
  let test s = f2 s |> printf "%d" in
  test "^v";
  [%expect {| 3 |}];
  test "^>v<";
  [%expect {| 3 |}];
  test "^v^v^v^v^v";
  [%expect {| 11 |}]

let run () = Lib.run ~name:"day03" ~f1 ~f2 Input.data
