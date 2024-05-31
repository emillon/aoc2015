open Base
open Stdio

module Cmdliner_let_syntax = struct
  open Cmdliner.Term

  let ( let+ ) x k = const k $ x
end

let run ~name ~f1 ~f2 data =
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
    printf "%d\n" (f data)
  in
  let cmd = Cmdliner.Cmd.v info term in
  Cmdliner.Cmd.eval cmd |> Stdlib.exit

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
