let parse_row s =
  String.to_list s
  |> Array.of_list_map ~f:(function
       | '#' -> true
       | '.' -> false
       | c -> raise_s [%message (c : char)])

let parse s = String.split_lines s |> Array.of_list_map ~f:parse_row
let alive b (x, y) = 0 <= x && x < 100 && 0 <= y && y < 100 && Board.get b (x, y)
let count_alive b pos = List.count (Pos.neighbours8 pos) ~f:(alive b)

let next b =
  Board.init ~dimx:100 ~dimy:100 ~f:(fun pos ->
      match (alive b pos, count_alive b pos) with
      | true, (2 | 3) -> true
      | false, 3 -> true
      | _ -> false)

let count = Board.fold ~init:0 ~f:(fun acc b -> if b then acc + 1 else acc)

let f1 s =
  let init = parse s in
  Fn.apply_n_times ~n:100 next init |> count

let next2 b =
  let r = next b in
  let corners = [ (0, 0); (0, 99); (99, 0); (99, 99) ] in
  List.iter corners ~f:(fun pos -> Board.set r pos true);
  r

let f2 s =
  let init = parse s in
  Fn.apply_n_times ~n:100 next2 init |> count

let run () = Run.run ~name:"day18" ~f1 ~f2 Day18_input.data
