open Algo
open Parsing_util

type creature = { hp : int; damage : int; armor : int }

let attack t ~target =
  let damage_dealt = Int.max 1 (t.damage - target.armor) in
  { target with hp = target.hp - damage_dealt }

let alive t = t.hp > 0

let rec victory ~player ~boss =
  let boss = attack player ~target:boss in
  (not (alive boss))
  ||
  let player = attack boss ~target:player in
  alive player && victory ~player ~boss

let%expect_test "victory" =
  victory
    ~player:{ hp = 8; damage = 5; armor = 5 }
    ~boss:{ hp = 12; damage = 7; armor = 2 }
  |> [%sexp_of: bool] |> print_s;
  [%expect {| true |}]

type item = { cost : int; bonus_damage : int; bonus_armor : int }
[@@deriving equal, sexp]

let optional l = [] :: List.map ~f:List.return l

let upto2 l =
  let l0 = [] in
  let l1 = List.map l ~f:(fun x -> [ x ]) in
  let l2 =
    let open List.Let_syntax in
    let%bind x1 = l in
    let%bind x2 = l in
    let%bind () = guard (not (equal_item x1 x2)) in
    [ [ x1; x2 ] ]
  in
  l0 @ l1 @ l2

let one = List.map ~f:List.return

let equipment_sets =
  let open List.Let_syntax in
  let%bind weapons =
    one
      [
        { cost = 8; bonus_damage = 4; bonus_armor = 0 };
        { cost = 10; bonus_damage = 5; bonus_armor = 0 };
        { cost = 25; bonus_damage = 6; bonus_armor = 0 };
        { cost = 40; bonus_damage = 7; bonus_armor = 0 };
        { cost = 74; bonus_damage = 8; bonus_armor = 0 };
      ]
  in
  let%bind armors =
    optional
      [
        { cost = 13; bonus_armor = 1; bonus_damage = 0 };
        { cost = 31; bonus_armor = 2; bonus_damage = 0 };
        { cost = 53; bonus_armor = 3; bonus_damage = 0 };
        { cost = 75; bonus_armor = 4; bonus_damage = 0 };
        { cost = 102; bonus_armor = 5; bonus_damage = 0 };
      ]
  in
  let%bind rings =
    upto2
      [
        { cost = 25; bonus_damage = 1; bonus_armor = 0 };
        { cost = 50; bonus_damage = 2; bonus_armor = 0 };
        { cost = 100; bonus_damage = 3; bonus_armor = 0 };
        { cost = 20; bonus_damage = 0; bonus_armor = 1 };
        { cost = 40; bonus_damage = 0; bonus_armor = 2 };
        { cost = 80; bonus_damage = 0; bonus_armor = 3 };
      ]
  in
  return (weapons @ armors @ rings)

let add_bonus p { bonus_armor; bonus_damage; cost = _ } =
  { p with armor = p.armor + bonus_armor; damage = p.damage + bonus_damage }

let total_cost set = List.fold set ~init:0 ~f:(fun a b -> a + b.cost)

let equip set =
  List.fold set ~init:{ hp = 100; armor = 0; damage = 0 } ~f:add_bonus

let parse =
  let line key =
    let open Angstrom in
    string key *> string ": " *> number <* end_of_line
  in
  parse_using
  @@
  let open Angstrom in
  let+ hp = line "Hit Points"
  and+ damage = line "Damage"
  and+ armor = line "Armor" in
  { hp; damage; armor }

let f1 s =
  let boss = parse s in
  List.min_elt ~compare:Int.compare
    (let open List.Let_syntax in
     let%bind set = equipment_sets in
     let player = equip set in
     let%bind () = guard (victory ~player ~boss) in
     [ total_cost set ])
  |> Option.value_exn

let f2 s =
  let boss = parse s in
  List.max_elt ~compare:Int.compare
    (let open List.Let_syntax in
     let%bind set = equipment_sets in
     let player = equip set in
     let%bind () = guard (not (victory ~player ~boss)) in
     [ total_cost set ])
  |> Option.value_exn

let run () = Run.run ~name:"day21" ~f1 ~f2 Day21_input.data
