open Parsing_util

type op = Turn_on | Turn_off | Toggle [@@deriving sexp]
type instruction = op * int * int [@@deriving sexp]

let key x y = (x * 1000) + y

let parse s =
  let position =
    let open Angstrom in
    let+ x = number <* string "," and+ y = number in
    key x y
  in
  let op =
    let open Angstrom in
    string "turn on" *> return Turn_on
    <|> string "turn off" *> return Turn_off
    <|> string "toggle" *> return Toggle
  in
  let instruction =
    let open Angstrom in
    let+ op = op <* string " "
    and+ a = position <* string " through "
    and+ b = position in
    (op, a, b)
  in
  Angstrom.parse_string ~consume:All instruction s |> Result.ok_or_failwith

let%expect_test "parse" =
  let test s = parse s |> [%sexp_of: instruction] |> print_s in
  test "turn on 0,0 through 999,999";
  [%expect {| (Turn_on 0 999999) |}];
  test "toggle 0,0 through 999,0";
  [%expect {| (Toggle 0 999000) |}];
  test "turn off 499,499 through 500,500";
  [%expect {| (Turn_off 499499 500500) |}]

let quorem a = (a / 1000, Int.rem a 1000)

let iter_rectangle a b ~f =
  let ax, ay = quorem a in
  let bx, by = quorem b in
  for x = ax to bx do
    for y = ay to by do
      f (key x y)
    done
  done

let interpret s (op, a, b) =
  match op with
  | Turn_on -> iter_rectangle a b ~f:(Hash_set.add s)
  | Turn_off -> iter_rectangle a b ~f:(Hash_set.remove s)
  | Toggle ->
      iter_rectangle a b ~f:(fun pos ->
          if Hash_set.mem s pos then Hash_set.remove s pos
          else Hash_set.add s pos)

let f1 s =
  let set = Hash_set.create (module Int) in
  String.split_lines s |> List.map ~f:parse |> List.iter ~f:(interpret set);
  Hash_set.length set

let interpret2 t (op, a, b) =
  iter_rectangle a b ~f:(fun pos ->
      Hashtbl.update t pos ~f:(fun no ->
          let n = Option.value no ~default:0 in
          match op with
          | Turn_on -> n + 1
          | Turn_off -> Int.max 0 (n - 1)
          | Toggle -> n + 2))

let f2 s =
  let t = Hashtbl.create (module Int) in
  String.split_lines s |> List.map ~f:parse |> List.iter ~f:(interpret2 t);
  Hashtbl.fold t ~init:0 ~f:(fun ~key:_ ~data n -> data + n)

let run () = Run.run ~name:"day06" ~f1 ~f2 Day06_input.data
