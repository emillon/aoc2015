open Algo
open Parsing_util

let parse =
  parse_using
  @@
  let open Angstrom in
  let+ src = word <* string " to "
  and+ dst = word <* string " = "
  and+ w = number in
  (src, dst, w)

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

let f1 s = best_by s ~f:(List.min_elt ~compare:Int.compare)
let f2 s = best_by s ~f:(List.max_elt ~compare:Int.compare)
let run () = Run.run ~name:"day09" ~f1 ~f2 Day09_input.data
