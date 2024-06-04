open Parsing_util

type prop =
  | Children
  | Cars
  | Vizslas
  | Akitas
  | Perfumes
  | Pomeranians
  | Goldfish
  | Cats
  | Trees
  | Samoyeds

type sue = { number : int; props : (prop * int) list }

let parse_line =
  let prop =
    let open Angstrom in
    let+ name =
      enum
        [
          ("children", Children);
          ("cars", Cars);
          ("vizslas", Vizslas);
          ("akitas", Akitas);
          ("perfumes", Perfumes);
          ("pomeranians", Pomeranians);
          ("goldfish", Goldfish);
          ("cats", Cats);
          ("trees", Trees);
          ("samoyeds", Samoyeds);
        ]
      <* string ": "
    and+ n = number in
    (name, n)
  in
  parse_using
  @@
  let open Angstrom in
  let+ number = string "Sue " *> number <* string ": "
  and+ props = sep_by1 (string ", ") prop in
  { number; props }

let expected_value = function
  | Children -> 3
  | Cars -> 2
  | Vizslas -> 0
  | Akitas -> 0
  | Perfumes -> 1
  | Pomeranians -> 3
  | Goldfish -> 5
  | Cats -> 7
  | Trees -> 3
  | Samoyeds -> 2

let consistent_p1 (prop, n) = expected_value prop = n

let consistent_p2 (prop, n) =
  match prop with
  | Cats | Trees -> n > expected_value prop
  | Pomeranians | Goldfish -> n < expected_value prop
  | Children | Cars | Vizslas | Akitas | Perfumes | Samoyeds ->
      n = expected_value prop

let f_gen s ~f =
  String.split_lines s |> List.map ~f:parse_line
  |> List.filter ~f:(fun s -> List.for_all s.props ~f)
  |> function
  | [ s ] -> s.number
  | _ -> assert false

let f1 = f_gen ~f:consistent_p1
let f2 = f_gen ~f:consistent_p2
let run () = Run.run ~name:"day16" ~f1 ~f2 Day16_input.data
