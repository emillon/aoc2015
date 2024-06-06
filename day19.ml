open Parsing_util

type atom = string [@@deriving compare, equal, sexp]

module Molecule = struct
  module T = struct
    type t = atom list [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let to_string l =
    let s = String.concat l in
    (s, String.Search_pattern.create s, List.length l)
end

type 'a rule = { src : atom; dst : 'a } [@@deriving sexp]

let map_rule ~f { src; dst } = { src; dst = f dst }

type 'a t = { rules : 'a rule list; start : 'a } [@@deriving sexp]

let map ~f { rules; start } =
  { rules = List.map rules ~f:(map_rule ~f); start = f start }

let parse =
  let atom =
    let open Angstrom in
    string "e"
    <|>
    let* c1 = satisfy Char.is_uppercase in
    let* c2o =
      option None
        (let+ c = satisfy Char.is_lowercase in
         Some c)
    in
    let char_opt () (opt : char option) =
      match opt with None -> "" | Some c -> Char.to_string c
    in
    return (Printf.sprintf "%c%a" c1 char_opt c2o)
  in
  let molecule =
    let open Angstrom in
    many1 atom
  in
  let rule =
    let open Angstrom in
    let+ src = atom <* string " => " and+ dst = molecule in
    { src; dst }
  in
  parse_using
  @@
  let open Angstrom in
  let+ rules = many_till (rule <* end_of_line) end_of_line
  and+ start = molecule <* end_of_line in
  { rules; start }

let sample = String.concat_lines [ "H => HO"; "H => OH"; "O => HH"; ""; "HOH" ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: Molecule.t t] |> print_s;
  [%expect
    {|
    ((rules (((src H) (dst (H O))) ((src H) (dst (O H))) ((src O) (dst (H H)))))
     (start (H O H))) |}]

let rec apply_rule rule m =
  match m with
  | atom :: atoms ->
      let r = List.map ~f:(List.cons atom) (apply_rule rule atoms) in
      if equal_atom atom rule.src then (rule.dst @ atoms) :: r else r
  | [] -> []

let apply_rules rules m =
  List.fold rules
    ~init:(Set.empty (module Molecule))
    ~f:(fun acc rule ->
      Set.union acc (Set.of_list (module Molecule) (apply_rule rule m)))

let%expect_test "apply_rules" =
  let { rules; start } = parse sample in
  apply_rules rules start |> [%sexp_of: Set.M(Molecule).t] |> print_s;
  [%expect {| ((H H H H) (H O H O) (H O O H) (O H O H)) |}]

let f1 s =
  let { rules; start } = parse s in
  Set.length (apply_rules rules start)

let%expect_test "f1" =
  f1 sample |> printf "%d";
  [%expect {| 4 |}]

let apply_rule_reverse rule s =
  let _, pat, _ = rule.dst in
  let indices = String.Search_pattern.index_all pat ~may_overlap:true ~in_:s in
  let src = match rule.src with "e" -> "" | s -> s in
  List.map indices ~f:(fun pos ->
      String.Search_pattern.replace_first pat ~pos ~in_:s ~with_:src)

let f2 s =
  let { rules; start = target, _, _ } = parse s |> map ~f:Molecule.to_string in
  let exception Found of int in
  (* TODO: this might not be the shortest one? *)
  let found i = raise (Found i) in
  let rec go s i =
    if String.is_empty s then found i
    else
      List.iter rules ~f:(fun rule ->
          let smaller = apply_rule_reverse rule s in
          List.iter smaller ~f:(fun s -> go s (i + 1)))
  in
  try
    go target 0;
    assert false
  with Found i -> i

let sample2 =
  String.concat_lines
    [ "e => H"; "e => O"; "H => HO"; "H => OH"; "O => HH"; ""; "HOH" ]

let%expect_test "f2" =
  f2 sample2 |> printf "%d";
  [%expect {| 3 |}]

let run () = Run.run ~name:"day19" ~f1 ~f2 Day19_input.data
