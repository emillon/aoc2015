open Algo

let subset_sum l n =
  let result = ref [] in
  let found l = result := l :: !result in
  let rec go ~current_sum ~picked ~left =
    match Int.compare current_sum n with
    | n when n > 0 -> ()
    | 0 -> found picked
    | _ -> (
        match left with
        | [] -> ()
        | x :: left ->
            go ~left ~picked:(x :: picked) ~current_sum:(current_sum + x);
            go ~left ~picked ~current_sum)
  in
  go ~left:l ~picked:[] ~current_sum:0;
  !result

let parts3 l =
  let n = sum l / 3 in
  subset_sum l n

let%expect_test _ =
  parts3 [ 1; 2; 3; 4; 5; 7; 8; 9; 10; 11 ]
  |> [%sexp_of: int list list] |> print_s;
  [%expect
    {|
    ((11 9) (8 7 5) (9 7 4) (11 5 4) (9 8 3) (10 7 3) (8 5 4 3) (10 8 2)
     (11 7 2) (9 5 4 2) (8 7 3 2) (10 5 3 2) (11 4 3 2) (10 9 1) (11 8 1)
     (8 7 4 1) (10 5 4 1) (9 7 3 1) (11 5 3 1) (7 5 4 3 1) (9 8 2 1) (10 7 2 1)
     (8 5 4 2 1) (9 5 3 2 1) (10 4 3 2 1)) |}]

let parse s = s |> String.split_lines |> List.map ~f:Int.of_string

let f_gen f s =
  parse s |> f
  |> List.sort_and_group ~compare:Stdlib.List.compare_lengths
  |> List.hd_exn |> List.map ~f:product
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

let f1 s = f_gen parts3 s

let parts4 l =
  let n = sum l / 4 in
  subset_sum l n

let f2 s = f_gen parts4 s
let run () = Run.run ~name:"day24" ~f1 ~f2 Day24_input.data
