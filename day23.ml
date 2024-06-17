open Parsing_util

type reg = A | B [@@deriving sexp]

type instr =
  | Inc of reg
  | Hlf of reg
  | Jio of reg * int
  | Tpl of reg
  | Jmp of int
  | Jie of reg * int
  | Halt
[@@deriving sexp]

type prog = instr Map.M(Int).t [@@deriving sexp]

let parse_line =
  let reg = enum [ ("a", A); ("b", B) ] in
  let offset =
    let open Angstrom in
    let+ sign_is_plus = enum [ ("+", true); ("-", false) ] and+ number in
    if sign_is_plus then number else -number
  in
  parse_using
  @@
  let open Angstrom in
  choice
    [
      (let+ _ = string "hlf " and+ reg in
       Hlf reg);
      (let+ _ = string "inc " and+ reg in
       Inc reg);
      (let+ reg = string "tpl " *> reg in
       Tpl reg);
      (let+ _ = string "jmp " and+ offset in
       Jmp offset);
      (let+ reg = string "jio " *> reg and+ offset = string ", " *> offset in
       Jio (reg, offset));
      (let+ reg = string "jie " *> reg and+ offset = string ", " *> offset in
       Jie (reg, offset));
    ]

let add_halt p =
  let max, _ = Map.max_elt_exn p in
  Map.add_exn ~key:(max + 1) ~data:Halt p

let parse s =
  String.split_lines s
  |> List.mapi ~f:(fun i s -> (i, parse_line s))
  |> Map.of_alist_exn (module Int)
  |> add_halt

let sample = String.concat_lines [ "inc a"; "jio a, +2"; "tpl a"; "inc a" ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: prog] |> print_s;
  [%expect {| ((0 (Inc A)) (1 (Jio A 2)) (2 (Tpl A)) (3 (Inc A)) (4 Halt)) |}]

type env = { a : int; b : int; pc : int }

let initial_env = { a = 0; b = 0; pc = 0 }
let reg e r = match r with A -> e.a | B -> e.b

let map_reg e ~f = function
  | A -> { e with a = f e.a }
  | B -> { e with b = f e.b }

let rec interp p e =
  let jump d = interp p { e with pc = e.pc + d } in
  let continue e = interp p { e with pc = e.pc + 1 } in
  let cjump e b d = if b then jump d else continue e in
  match Map.find_exn p e.pc with
  | Hlf r -> map_reg e r ~f:(fun n -> n / 2) |> continue
  | Inc r -> map_reg e r ~f:Int.succ |> continue
  | Tpl r -> map_reg e r ~f:(fun n -> n * 3) |> continue
  | Jmp d -> jump d
  | Jie (r, d) -> cjump e (reg e r % 2 = 0) d
  | Jio (r, d) -> cjump e (reg e r = 1) d
  | Halt -> e

let%expect_test "interp" =
  let p = parse sample in
  let e = interp p initial_env in
  printf "%d" e.a;
  [%expect {| 2 |}]

let f1 s =
  let p = parse s in
  let e = interp p initial_env in
  e.b

let f2 s =
  let p = parse s in
  let e = interp p { initial_env with a = 1 } in
  e.b

let run () = Run.run ~name:"day23" ~f1 ~f2 Day23_input.data
