let find_prime_factor0 n =
  let rec go c =
    if c ** 2 > n then None else if n % c = 0 then Some c else go (c + 1)
  in
  go 2

let prime_multiplicity n p =
  let rec go m r = if m % p = 0 then go (m / p) (r + 1) else (r, m) in
  go n 0

let find_prime_factor n =
  match find_prime_factor0 n with
  | None -> None
  | Some p ->
      let m, d = prime_multiplicity n p in
      Some (p, m, d)

let presents_nocache n f =
  match find_prime_factor n with
  | Some (p, m, d) -> ((p ** (m + 1)) - 1) / (p - 1) * f d
  | None when n = 1 -> 1
  | None -> n + 1

let rec presents =
  let cache = ref (Map.empty (module Int)) in
  fun n ->
    match Map.find !cache n with
    | Some r -> r
    | None ->
        let r = presents_nocache n presents in
        cache := Map.add_exn !cache ~key:n ~data:r;
        r

let presents n = 10 * presents n

let%expect_test "presents" =
  let test n = presents n |> printf "%d" in
  test 1;
  [%expect {| 10 |}];
  test 2;
  [%expect {| 30 |}];
  test 3;
  [%expect {| 40 |}];
  test 4;
  [%expect {| 70 |}];
  test 5;
  [%expect {| 60 |}];
  test 6;
  [%expect {| 120 |}];
  test 7;
  [%expect {| 80 |}];
  test 8;
  [%expect {| 150 |}];
  test 9;
  [%expect {| 130 |}];
  test 12;
  [%expect {| 280 |}];
  test 36;
  [%expect {| 910 |}]

let f1 s =
  let n = String.strip s |> Int.of_string in
  let rec go i =
    let r = presents i in
    if r >= n then i else go (i + 1)
  in
  go 1

let f2 _ = 0
let run () = Run.run ~name:"day20" ~f1 ~f2 Day20_input.data
