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

let neighbours8 (x, y) =
  [
    (x - 1, y - 1);
    (x, y - 1);
    (x + 1, y - 1);
    (x - 1, y);
    (x + 1, y);
    (x - 1, y + 1);
    (x, y + 1);
    (x + 1, y + 1);
  ]
