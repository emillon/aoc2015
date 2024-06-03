open Parsing_util

type reindeer = { speed : int; flight_duration : int; rest_duration : int }
[@@deriving sexp]

let parse_line =
  parse_using
  @@
  let open Angstrom in
  let+ name = word <* string " can fly "
  and+ speed = number <* string " km/s for "
  and+ flight_duration = number <* string " seconds, but then must rest for "
  and+ rest_duration = number <* string " seconds." in
  (name, { speed; flight_duration; rest_duration })

let parse s =
  String.split_lines s |> List.map ~f:parse_line
  |> Map.of_alist_exn (module String)

let sample =
  String.concat_lines
    [
      "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 \
       seconds.";
      "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 \
       seconds.";
    ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: reindeer Map.M(String).t] |> print_s;
  [%expect
    {|
    ((Comet ((speed 14) (flight_duration 10) (rest_duration 127)))
     (Dancer ((speed 16) (flight_duration 11) (rest_duration 162)))) |}]

type state = {
  reindeer : reindeer;
  pos : int;
  score : int;
  is_flying : bool;
  leftover : int;
}

let pass_time state = { state with leftover = state.leftover - 1 }

let state_change state =
  match state with
  | { leftover = 0; is_flying = true; _ } ->
      { state with is_flying = false; leftover = state.reindeer.rest_duration }
  | { leftover = 0; is_flying = false; _ } ->
      { state with is_flying = true; leftover = state.reindeer.flight_duration }
  | _ -> state

let move state =
  match state.is_flying with
  | true -> { state with pos = state.pos + state.reindeer.speed }
  | false -> state

let next state = state |> pass_time |> move |> state_change

let simulate ~n r =
  let s =
    Fn.apply_n_times ~n
      (fun state -> state |> pass_time |> move |> state_change)
      {
        reindeer = r;
        pos = 0;
        score = 0;
        leftover = r.flight_duration;
        is_flying = true;
      }
  in
  s.pos

let simulate_all n = Map.map ~f:(simulate ~n)

let%expect_test "simulate" =
  let m = parse sample in
  simulate_all 1000 m |> [%sexp_of: int Map.M(String).t] |> print_s;
  [%expect {| ((Comet 1120) (Dancer 1056)) |}]

let max_value =
  Map.fold ~init:Int.min_value ~f:(fun ~key:_ ~data acc -> Int.max data acc)

let f1 s = parse s |> simulate_all 2503 |> max_value

let award_points m =
  let max_pos =
    Map.fold m ~init:Int.min_value ~f:(fun ~key:_ ~data acc ->
        Int.max acc data.pos)
  in
  Map.map m ~f:(fun rs ->
      if rs.pos = max_pos then { rs with score = rs.score + 1 } else rs)

let simulate2_all ~n (m : reindeer Map.M(String).t) =
  let init_state_map =
    Map.map m ~f:(fun r ->
        {
          reindeer = r;
          leftover = r.flight_duration;
          is_flying = true;
          pos = 0;
          score = 0;
        })
  in
  let go state_map = Map.map state_map ~f:next |> award_points in
  Fn.apply_n_times ~n go init_state_map |> Map.map ~f:(fun s -> s.score)

let%expect_test "simulate2" =
  parse sample |> simulate2_all ~n:1000 |> [%sexp_of: int Map.M(String).t]
  |> print_s;
  [%expect {| ((Comet 312) (Dancer 689)) |}]

let f2 s = parse s |> simulate2_all ~n:2503 |> max_value
let run () = Run.run ~name:"day14" ~f1 ~f2 Day14_input.data
