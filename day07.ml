open Parsing_util

type op = And | Or | Lshift | Rshift [@@deriving compare, equal, hash, sexp]

type exp = Int of int | Var of string | Op of op * exp * exp | Not of exp
[@@deriving compare, equal, hash, sexp]

module Definition = struct
  type t = string * exp [@@deriving compare, equal, hash, sexp]
end

type defs = exp Map.M(String).t [@@deriving sexp]

let rec vars_in = function
  | Int _ -> []
  | Var v -> [ v ]
  | Op (_, a, b) -> vars_in a @ vars_in b
  | Not e -> vars_in e

let parse_line =
  let op =
    let open Angstrom in
    choice
      [
        string " AND " *> return And;
        string " OR " *> return Or;
        string " LSHIFT " *> return Lshift;
        string " RSHIFT " *> return Rshift;
      ]
  in
  let atom =
    let open Angstrom in
    choice
      [
        (let+ n = number in
         Int n);
        (let+ s = word in
         Var s);
      ]
  in
  let exp =
    let open Angstrom in
    choice
      [
        (let+ a = atom and+ op = op and+ b = atom in
         Op (op, a, b));
        (let+ a = string "NOT " *> atom in
         Not a);
        atom;
      ]
  in
  parse_using
  @@
  let open Angstrom in
  let+ exp = exp <* string " -> " and+ var = word in
  (var, exp)

let parse l = List.map l ~f:parse_line |> Map.of_alist_exn (module String)

let eval_op op =
  match op with
  | Rshift -> ( lsr )
  | Lshift -> ( lsl )
  | Or -> ( lor )
  | And -> ( land )

let rec eval_exp env = function
  | Int n -> n
  | Var s -> Map.find_exn env s
  | Not e ->
      let ne = eval_exp env e in
      lnot ne land 0xff_ff
  | Op (op, a, b) ->
      let na = eval_exp env a in
      let nb = eval_exp env b in
      eval_op op na nb

module G = struct
  type t = exp Map.M(String).t

  module V = Definition

  let iter_vertex f g = Map.iteri g ~f:(fun ~key ~data -> f (key, data))
  let find_def g v = (v, Map.find_exn g v)

  let iter_succ f g (_, exp) =
    vars_in exp |> List.map ~f:(find_def g) |> List.iter ~f
end

let reorder dl =
  let module T = Graph.Topological.Make (G) in
  T.fold List.cons dl []

let eval dl =
  reorder dl
  |> List.fold
       ~init:(Map.empty (module String))
       ~f:(fun env (v, exp) ->
         let value = eval_exp env exp in
         Map.add_exn env ~key:v ~data:value)

let sample =
  String.concat_lines
    [
      "123 -> x";
      "456 -> y";
      "x AND y -> d";
      "x OR y -> e";
      "x LSHIFT 2 -> f";
      "y RSHIFT 2 -> g";
      "NOT x -> h";
      "NOT y -> i";
    ]

let%expect_test "parse and eval" =
  let dl = sample |> String.split_lines |> parse in
  dl |> [%sexp_of: defs] |> print_s;
  [%expect
    {|
    ((d (Op And (Var x) (Var y))) (e (Op Or (Var x) (Var y)))
     (f (Op Lshift (Var x) (Int 2))) (g (Op Rshift (Var y) (Int 2)))
     (h (Not (Var x))) (i (Not (Var y))) (x (Int 123)) (y (Int 456))) |}];
  eval dl |> [%sexp_of: int Map.M(String).t] |> print_s;
  [%expect
    {| ((d 72) (e 507) (f 492) (g 114) (h 65412) (i 65079) (x 123) (y 456)) |}]

let eval_full s =
  let dl = s |> String.split_lines |> parse in
  let env = eval dl in
  (dl, Map.find_exn env "a")

let f1 s = snd (eval_full s)

let f2 s =
  let dl, a = eval_full s in
  let dl2 = Map.set dl ~key:"b" ~data:(Int a) in
  let env2 = eval dl2 in
  Map.find_exn env2 "a"

let run () = Run.run ~name:"day07" ~f1 ~f2 Day07_input.data
