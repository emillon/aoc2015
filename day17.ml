open Algo

let ways n all = sublists all |> List.filter ~f:(fun l -> sum l = n)

let%expect_test "ways" =
  ways 25 [ 20; 15; 10; 5; 5 ] |> [%sexp_of: int list list] |> print_s;
  [%expect {| ((15 10) (20 5) (20 5) (15 5 5)) |}]

let parse s = String.split_lines s |> List.map ~f:Int.of_string
let ok_ways s = ways 150 (parse s)
let f1 s = List.length (ok_ways s)

let f2 s =
  let lens = List.map ~f:List.length (ok_ways s) in
  let min_len = List.min_elt lens ~compare:Int.compare |> Option.value_exn in
  List.count lens ~f:(fun len -> len = min_len)

let run () = Run.run ~name:"day17" ~f1 ~f2 Day17_input.data
