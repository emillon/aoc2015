open Base
open Algo

let has_red = List.exists ~f:(function _, `String "red" -> true | _ -> false)

let rec json_sum ~ignore_red = function
  | `Bool _ -> 0
  | `Assoc kvs ->
      if ignore_red && has_red kvs then 0
      else List.map kvs ~f:(fun (_, v) -> json_sum v ~ignore_red) |> sum
  | `Int n -> n
  | `List l -> List.map l ~f:(json_sum ~ignore_red) |> sum
  | `Null -> 0
  | `String _ -> 0
  | `Float _ -> assert false
  | `Intlit _ -> assert false
  | `Tuple _ -> assert false
  | `Variant _ -> assert false

let f1 s = Yojson.Safe.from_string s |> json_sum ~ignore_red:false
let f2 s = Yojson.Safe.from_string s |> json_sum ~ignore_red:true
let run () = Run.run ~name:"day12" ~f1 ~f2 Day12_input.data
