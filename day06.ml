open Parsing_util

type op = Turn_on | Turn_off | Toggle [@@deriving sexp]
type instruction = op * Pos.t * Pos.t [@@deriving sexp]

let parse =
  let position =
    let open Angstrom in
    let+ x = number <* string "," and+ y = number in
    (x, y)
  in
  let op =
    let open Angstrom in
    string "turn on" *> return Turn_on
    <|> string "turn off" *> return Turn_off
    <|> string "toggle" *> return Toggle
  in
  parse_using
  @@
  let open Angstrom in
  let+ op = op <* string " "
  and+ a = position <* string " through "
  and+ b = position in
  (op, a, b)

let%expect_test "parse" =
  let test s = parse s |> [%sexp_of: instruction] |> print_s in
  test "turn on 0,0 through 999,999";
  [%expect {| (Turn_on (0 0) (999 999)) |}];
  test "toggle 0,0 through 999,0";
  [%expect {| (Toggle (0 0) (999 0)) |}];
  test "turn off 499,499 through 500,500";
  [%expect {| (Turn_off (499 499) (500 500)) |}]

let iter_rectangle (ax, ay) (bx, by) ~f =
  for x = ax to bx do
    for y = ay to by do
      f (x, y)
    done
  done

let array_update t (x, y) ~f =
  let v = t.(x).(y) in
  let v' = f v in
  t.(x).(y) <- v'

let interpret t (op, a, b) =
  let f =
    match op with
    | Turn_on -> fun _ -> true
    | Turn_off -> fun _ -> false
    | Toggle -> not
  in
  iter_rectangle a b ~f:(array_update t ~f)

let array_fold_matrix t ~f ~init =
  Array.fold t ~init ~f:(fun acc ar -> Array.fold ar ~init:acc ~f)

let f1 s =
  let t = Array.make_matrix ~dimx:1000 ~dimy:1000 false in
  String.split_lines s |> List.map ~f:parse |> List.iter ~f:(interpret t);
  array_fold_matrix t ~init:0 ~f:(fun n b -> if b then n + 1 else n)

let interpret2 t (op, a, b) =
  let f =
    match op with
    | Turn_on -> fun n -> n + 1
    | Turn_off -> fun n -> Int.max 0 (n - 1)
    | Toggle -> fun n -> n + 2
  in
  iter_rectangle a b ~f:(array_update t ~f)

let f2 s =
  let t = Array.make_matrix ~dimx:1000 ~dimy:1000 0 in
  String.split_lines s |> List.map ~f:parse |> List.iter ~f:(interpret2 t);
  array_fold_matrix t ~init:0 ~f:( + )

let run () = Run.run ~name:"day06" ~f1 ~f2 Day06_input.data
