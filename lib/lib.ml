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
