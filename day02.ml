open Algo

let parse s = Stdlib.Scanf.sscanf s "%dx%dx%d" (fun l w h -> (l, w, h))
let smallest l = List.min_elt ~compare:Int.compare l |> Option.value_exn

let result (l, w, h) =
  let sides = [ l * w; w * h; h * l ] in
  (2 * sum sides) + smallest sides

let%expect_test "result" =
  let test s = printf "%d\n" (result (parse s)) in
  test "2x3x4";
  [%expect {| 58 |}];
  test "1x1x10";
  [%expect {| 43 |}]

let f1 data =
  String.split_lines data |> List.map ~f:(fun s -> result (parse s)) |> sum

let result2 (l, w, h) =
  let sides = [ (2 * l) + (2 * w); (2 * w) + (2 * h); (2 * l) + (2 * h) ] in
  let ribbon = l * w * h in
  smallest sides + ribbon

let%expect_test "result2" =
  let test s = printf "%d\n" (result2 (parse s)) in
  test "2x3x4";
  [%expect {| 34 |}];
  test "1x1x10";
  [%expect {| 14 |}]

let f2 data =
  String.split_lines data |> List.map ~f:(fun s -> result2 (parse s)) |> sum

let run () = Run.run ~name:"day02" ~f1 ~f2 Day02_input.data
