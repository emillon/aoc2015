open Base

type t = N | S | E | W

let of_char = function
  | '^' -> N
  | 'v' -> S
  | '>' -> E
  | '<' -> W
  | c -> raise_s [%message "Dir.of_char" (c : char)]
