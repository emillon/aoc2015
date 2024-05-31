open Base
open Stdio

module Cmdliner_let_syntax = struct
  open Cmdliner.Term

  let ( let+ ) x k = const k $ x
end

let run_gen ~name ~f1 ~f2 data ~print =
  let info = Cmdliner.Cmd.info name in
  let term =
    let open Cmdliner_let_syntax in
    let+ part =
      let open Cmdliner.Arg in
      required
      & opt (some (enum [ ("1", `One); ("2", `Two) ])) None
      & info [ "part" ]
    in
    let f = match part with `One -> f1 | `Two -> f2 in
    print (f data)
  in
  let cmd = Cmdliner.Cmd.v info term in
  Cmdliner.Cmd.eval cmd |> Stdlib.exit

let run ~name ~f1 ~f2 data = run_gen ~name ~f1 ~f2 data ~print:(printf "%d\n")

let run_string ~name ~f1 ~f2 data =
  run_gen ~name ~f1 ~f2 data ~print:(printf "%s\n")

module Dir = struct
  type t = N | S | E | W

  let of_char = function
    | '^' -> N
    | 'v' -> S
    | '>' -> E
    | '<' -> W
    | c -> raise_s [%message "Dir.of_char" (c : char)]
end

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let shift (x, y) (d : Dir.t) =
    match d with
    | N -> (x, y - 1)
    | S -> (x, y + 1)
    | E -> (x + 1, y)
    | W -> (x - 1, y)
end

let number =
  let open Angstrom in
  let+ s = take_while1 Char.is_digit in
  Int.of_string s

let sum = List.fold ~init:0 ~f:( + )

let rec insert x l =
  match l with
  | [] -> [ [ x ] ]
  | h :: t -> (x :: l) :: List.map ~f:(fun l -> h :: l) (insert x t)

let rec permutations = function
  | [] -> [ [] ]
  | x :: xs -> List.concat_map ~f:(insert x) (permutations xs)

let%expect_test "permutations" =
  permutations [ 1; 2; 3; 4 ] |> [%sexp_of: int list list] |> print_s;
  [%expect
    {|
    ((1 2 3 4) (2 1 3 4) (2 3 1 4) (2 3 4 1) (1 3 2 4) (3 1 2 4) (3 2 1 4)
     (3 2 4 1) (1 3 4 2) (3 1 4 2) (3 4 1 2) (3 4 2 1) (1 2 4 3) (2 1 4 3)
     (2 4 1 3) (2 4 3 1) (1 4 2 3) (4 1 2 3) (4 2 1 3) (4 2 3 1) (1 4 3 2)
     (4 1 3 2) (4 3 1 2) (4 3 2 1)) |}]

let rec legs_after x = function a :: l -> (x, a) :: legs_after a l | [] -> []
let legs = function [] -> assert false | x :: xs -> legs_after x xs

module type Science_input = sig
  type t [@@deriving sexp_of]
end

module type Science_output = sig
  type t [@@deriving equal, sexp_of]
end

let science (type input output) ?(bypass = false) ~control ~experiment
    (module Input : Science_input with type t = input)
    (module Output : Science_output with type t = output) x =
  let control = control x in
  if bypass then control
  else
    let exp = experiment x in
    if not (Output.equal control exp) then
      raise_s [%message (x : Input.t) (control : Output.t) (exp : Output.t)];
    control

exception Overflow

let char_succ_exn c = Char.to_int c |> Int.succ |> Char.of_int_exn

let rec incr_buf_at_point ~min ~max b i =
  if i < 0 then raise Overflow;
  let c = Bytes.get b i in
  if Char.equal c max then (
    Bytes.set b i min;
    incr_buf_at_point ~min ~max b (i - 1))
  else Bytes.set b i (char_succ_exn c)

let incr_buf ~min ~max b = incr_buf_at_point ~min ~max b (Bytes.length b - 1)

let resize_b ~min b =
  let r = Bytes.make (Bytes.length b + 1) min in
  Bytes.set r 0 (char_succ_exn min);
  r

let iter_bytes ~f ~start ~min ~max =
  let rec go b =
    if f b then Bytes.to_string b
    else
      match incr_buf ~min ~max b with
      | () -> go b
      | exception Overflow -> go (resize_b ~min b)
  in
  go (Bytes.of_string start)

let%expect_test "iter_bytes" =
  iter_bytes
    ~f:(fun b ->
      let s = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b in
      printf "%s\n" s;
      Int.of_string s >= 12)
    ~start:"8" ~min:'0' ~max:'9'
  |> printf "result: %s";
  [%expect {|
    8
    9
    10
    11
    12
    result: 12 |}]
