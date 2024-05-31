open Algo
open Parsing_util

let parse s =
  let line =
    let open Angstrom in
    let+ src = word <* string " to "
    and+ dst = word <* string " = "
    and+ w = number in
    (src, dst, w)
  in
  Angstrom.parse_string ~consume:All line s |> Result.ok_or_failwith

module Key = struct
  module T = struct
    type t = string * string [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let best_by s ~f =
  let map, cities =
    String.split_lines s
    |> List.fold
         ~init:(Map.empty (module Key), Set.empty (module String))
         ~f:(fun (map, cities) s ->
           let src, dst, data = parse s in
           ( map
             |> Map.add_exn ~key:(src, dst) ~data
             |> Map.add_exn ~key:(dst, src) ~data,
             Set.add (Set.add cities dst) src ))
  in
  let eval_leg k = Map.find_exn map k in
  let eval l = legs l |> List.map ~f:eval_leg |> sum in
  Set.to_list cities |> permutations |> List.map ~f:eval |> f
  |> Option.value_exn

let%expect_test "part 1" =
  best_by Day09_input.data ~f:(List.min_elt ~compare:Int.compare) |> printf "%d";
  [%expect {| 117 |}]

let%expect_test "part 2" =
  best_by Day09_input.data ~f:(List.max_elt ~compare:Int.compare) |> printf "%d";
  [%expect {| 909 |}]
