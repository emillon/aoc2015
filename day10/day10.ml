open Base

let look_and_say s =
  String.to_list s
  |> List.group ~break:(fun a b -> not (Char.equal a b))
  |> List.map ~f:(fun g ->
         Printf.sprintf "%d%c" (List.length g) (List.hd_exn g))
  |> String.concat

let f1 s = Fn.apply_n_times ~n:40 look_and_say s |> String.length
let f2 s = Fn.apply_n_times ~n:50 look_and_say s |> String.length
let run () = Lib.run ~name:"day10" ~f1 ~f2 "3113322113"
