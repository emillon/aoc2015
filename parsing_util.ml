let number =
  let open Angstrom in
  let+ s = take_while1 Char.is_digit in
  Int.of_string s
