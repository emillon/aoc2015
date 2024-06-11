open Parsing_util

module Positive_int : sig
  type t = private int [@@deriving sexp_of]

  val of_int_exn : int -> t
  val decrease : t -> int -> t option
  val increase : t -> int -> t
end = struct
  type t = int [@@deriving sexp_of]

  let of_int_exn n =
    assert (n > 0);
    n

  let decrease a b =
    assert (b > 0);
    let r = a - b in
    Option.some_if (r > 0) r

  let increase a b =
    assert (b > 0);
    a + b
end

type creature = { hp : int; damage : int; mana : int } [@@deriving sexp_of]

type action = Magic_missile | Drain | Shield | Poison | Recharge
[@@deriving sexp_of]

let action_string = function
  | Magic_missile -> "missile"
  | Drain -> "drain"
  | Shield -> "shield"
  | Poison -> "poison"
  | Recharge -> "recharge"

let all_actions = [ Magic_missile; Drain; Shield; Poison; Recharge ]

let action_cost = function
  | Magic_missile -> 53
  | Drain -> 73
  | Shield -> 113
  | Poison -> 173
  | Recharge -> 229

module Effect_type = struct
  module T = struct
    type t = Eff_shield | Eff_poison | Eff_recharge [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let duration = function
    | Eff_shield -> 6
    | Eff_poison -> 6
    | Eff_recharge -> 5

  let to_char = function
    | Eff_shield -> 'S'
    | Eff_poison -> 'P'
    | Eff_recharge -> 'R'
end

let effect_line m =
  Map.to_alist m
  |> List.map ~f:(fun (e, n) -> Printf.sprintf "%c%d" (Effect_type.to_char e) n)
  |> String.concat

type turn = Player | Boss [@@deriving sexp_of]

type state = {
  player : creature;
  boss : creature;
  effects : int Map.M(Effect_type).t;
  trace : action list;
  turn : turn;
  score : int option;
  hard : bool;
}
[@@deriving sexp_of]

type invalid = Boss_dead of int | Duplicate_effect | No_mana | Player_dead
[@@deriving sexp_of]

let trace_score l =
  List.fold l ~init:0 ~f:(fun acc act -> acc + action_cost act)

let damage_boss state damage =
  let hp = state.boss.hp - damage in
  if hp > 0 then Ok { state with boss = { state.boss with hp } }
  else Error (Boss_dead (trace_score state.trace))

let heal_player state ~hp =
  { state with player = { state.player with hp = state.player.hp + hp } }

let get_mana state n =
  { state with player = { state.player with mana = state.player.mana + n } }

let player_turn_apply_effect state =
  Map.fold state.effects ~init:(Ok state) ~f:(fun ~key ~data:_ state_e ->
      let open Result.Let_syntax in
      let%bind state = state_e in
      match key with
      | Eff_shield -> Ok state
      | Eff_poison -> damage_boss state 3
      | Eff_recharge -> Ok (get_mana state 101))

let player_turn_fade_effect state =
  let effects =
    Map.filter_map state.effects ~f:(function 1 -> None | n -> Some (n - 1))
  in
  { state with effects }

let start_effect state type_ =
  match Map.add state.effects ~key:type_ ~data:(Effect_type.duration type_) with
  | `Duplicate -> Error Duplicate_effect
  | `Ok effects -> Ok { state with effects }

let player_spend_mana state ~action =
  let cost = action_cost action in
  let s =
    {
      state with
      player = { state.player with mana = state.player.mana - cost };
      trace = action :: state.trace;
    }
  in
  if s.player.mana < 0 then Error No_mana else Ok s

let player_turn_cast state ~action =
  let open Result.Let_syntax in
  match action with
  | Magic_missile -> damage_boss state 4
  | Drain ->
      let%map state = damage_boss state 2 in
      heal_player state ~hp:2
  | Shield -> start_effect state Eff_shield
  | Poison -> start_effect state Eff_poison
  | Recharge -> start_effect state Eff_recharge

let player_take_damage state amount =
  let hp = state.player.hp - amount in
  if hp > 0 then Ok { state with player = { state.player with hp } }
  else Error Player_dead

let apply_difficulty state =
  if state.hard then player_take_damage state 1 else Ok state

let player_turn state ~action =
  let open Result.Let_syntax in
  let%bind state = apply_difficulty state in
  let%bind state = player_spend_mana state ~action in
  player_turn_cast state ~action

let effective_player_armor state =
  if Map.mem state.effects Eff_shield then 7 else 0

let boss_hit state =
  let damage_dealt =
    Int.max 1 (state.boss.damage - effective_player_armor state)
  in
  player_take_damage state damage_dealt

let boss_turn state = boss_hit state

let parse =
  let line key =
    let open Angstrom in
    string key *> string ": " *> number <* end_of_line
  in
  parse_using
  @@
  let open Angstrom in
  let+ hp = line "Hit Points" and+ damage = line "Damage" in
  { hp; damage; mana = 0 }

let other_turn = function Boss -> Player | Player -> Boss
let toggle_turn state = { state with turn = other_turn state.turn }

let next_states state =
  match player_turn_apply_effect state with
  | Error (Boss_dead score) -> [ { state with score = Some score } ]
  | Error _ -> []
  | Ok state -> (
      let state = player_turn_fade_effect state in
      match state.turn with
      | Player ->
          List.filter_map all_actions ~f:(fun action ->
              match player_turn state ~action with
              | Ok state -> Some (toggle_turn state)
              | Error (Boss_dead score) ->
                  Some { state with score = Some score }
              | Error _ -> None)
      | Boss -> (
          match boss_turn state with
          | Ok state -> [ toggle_turn state ]
          | Error (Boss_dead score) -> [ { state with score = Some score } ]
          | Error _ -> []))

let best ~player ~boss ~hard =
  let q = Queue.create () in
  let init_state =
    {
      player;
      boss;
      effects = Map.empty (module Effect_type);
      trace = [];
      turn = Player;
      score = None;
      hard;
    }
  in
  let best_score = ref Int.max_value in
  Queue.enqueue q init_state;
  while not (Queue.is_empty q) do
    let s = Queue.dequeue_exn q in
    match s.score with
    | Some score -> best_score := Int.min !best_score score
    | None ->
        let n = next_states s in
        Queue.enqueue_all q n
  done;
  !best_score

let replay_trace ~player_hp ~player_mana ~boss_hp ~boss_damage trace =
  let init =
    {
      player = { hp = player_hp; damage = 0; mana = player_mana };
      boss = { hp = boss_hp; damage = boss_damage; mana = 0 };
      effects = Map.empty (module Effect_type);
      trace = [];
      turn = Player;
      score = None;
      hard = false;
    }
  in
  let unwrap = function
    | Ok s -> s
    | Error e ->
        print_s ([%sexp_of: invalid] e);
        assert false
  in
  let sim_effects0 state =
    let open Result.Let_syntax in
    let%map state = player_turn_apply_effect state in
    player_turn_fade_effect state
  in
  let sim_effects state = sim_effects0 state |> unwrap in
  let sim_player_turn state action = sim_effects state |> player_turn ~action in
  let sim_boss state =
    let open Result.Let_syntax in
    let%bind state = sim_effects0 state in
    boss_turn state
  in
  let ok = ref true in
  let sr = ref init in
  let q = Queue.of_list trace in
  while !ok do
    let s = !sr in
    let se =
      match s.turn with
      | Player ->
          let action = Queue.dequeue_exn q in
          printf "(%8s)  " (action_string action);
          sim_player_turn s action
      | Boss ->
          printf "boss        ";
          sim_boss s
    in
    match se with
    | Error e ->
        print_s ([%sexp_of: invalid] e);
        ok := false
    | Ok s ->
        printf "player HP: %2d   mana: %3d     boss HP: %2d   %s\n"
          (s.player.hp :> int)
          s.player.mana
          (s.boss.hp :> int)
          (effect_line s.effects);
        sr := toggle_turn s
  done

let%expect_test "traces" =
  replay_trace ~player_hp:10 ~player_mana:250 ~boss_hp:13 ~boss_damage:8
    [ Poison; Magic_missile ];
  [%expect
    {|
    (  poison)  player HP: 10   mana:  77     boss HP: 13   P6
    boss        player HP:  2   mana:  77     boss HP: 10   P5
    ( missile)  player HP:  2   mana:  24     boss HP:  3   P4
    boss        (Boss_dead 226) |}];
  replay_trace ~player_hp:10 ~player_mana:250 ~boss_hp:14 ~boss_damage:8
    [ Recharge; Shield; Drain; Poison; Magic_missile ];
  [%expect
    {|
    (recharge)  player HP: 10   mana:  21     boss HP: 14   R5
    boss        player HP:  2   mana: 122     boss HP: 14   R4
    (  shield)  player HP:  2   mana: 110     boss HP: 14   S6R3
    boss        player HP:  1   mana: 211     boss HP: 14   S5R2
    (   drain)  player HP:  3   mana: 239     boss HP: 12   S4R1
    boss        player HP:  2   mana: 340     boss HP: 12   S3
    (  poison)  player HP:  2   mana: 167     boss HP: 12   S2P6
    boss        player HP:  1   mana: 167     boss HP:  9   S1P5
    ( missile)  player HP:  1   mana: 114     boss HP:  2   P4
    boss        (Boss_dead 641) |}]

let%expect_test "best" =
  let test hp =
    best
      ~player:{ hp = 10; mana = 250; damage = 0 }
      ~boss:{ hp; damage = 8; mana = 0 }
      ~hard:false
    |> printf "%d"
  in
  test 13;
  [%expect {| 226 |}];
  test 14;
  [%expect {| 641 |}]

let f_diff s ~hard =
  let boss = parse s in
  let player = { hp = 50; damage = 0; mana = 500 } in
  best ~player ~boss ~hard

let f1 = f_diff ~hard:false
let f2 = f_diff ~hard:true
let run () = Run.run ~name:"day22" ~f1 ~f2 Day22_input.data
