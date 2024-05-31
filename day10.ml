let char_to_int c = Char.to_int c - Char.to_int '0'

let look_and_say l =
  let acc = ref [] in
  let flush (count, num) = acc := [ num; count ] @ !acc in
  List.fold_left l ~init:None ~f:(fun state num ->
      match state with
      | None -> Some (1, num)
      | Some ((count, t) as group) ->
          if t = num then Some (count + 1, t)
          else (
            flush group;
            Some (1, num)))
  |> Option.iter ~f:flush;
  List.rev !acc

let f_gen n s =
  String.to_list s |> List.map ~f:char_to_int
  |> Fn.apply_n_times ~n look_and_say
  |> List.length

let data = "3113322113"

let%expect_test "part 1" =
  printf "%d" (f_gen 40 data);
  [%expect {| 329356 |}]

let%expect_test "part 2" =
  printf "%d" (f_gen 50 data);
  [%expect {| 4666278 |}]
