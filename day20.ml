open Algo

let find_prime_factor0 n =
  let rec go c =
    if c ** 2 > n then None else if n % c = 0 then Some c else go (c + 1)
  in
  go 2

let prime_multiplicity n p =
  let rec go m r = if m % p = 0 then go (m / p) (r + 1) else (r, m) in
  go n 0

let find_prime_factor n =
  let%map.Option p = find_prime_factor0 n in
  let m, d = prime_multiplicity n p in
  (p, m, d)

let factors_nocache n f =
  match find_prime_factor n with
  | Some (p, m, d) -> (p, m) :: f d
  | None when n = 1 -> []
  | None -> [ (n, 1) ]

let rec factors =
  let cache = ref (Map.empty (module Int)) in
  fun n ->
    match Map.find !cache n with
    | Some r -> r
    | None ->
        let r = factors_nocache n factors in
        cache := Map.add_exn !cache ~key:n ~data:r;
        r

let presents n =
  let s1 =
    List.fold (factors n) ~init:1 ~f:(fun acc (p, m) ->
        ((p ** (m + 1)) - 1) / (p - 1) * acc)
  in
  10 * s1

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

let lowest_such_that ~f =
  let rec go i = if f i then i else go (i + 1) in
  go 1

let f1 s =
  let n = String.strip s |> Int.of_string in
  lowest_such_that ~f:(fun i -> presents i >= n)

let rec divisors_from_factors f =
  match f with
  | [] -> [ 1 ]
  | (p, m) :: d ->
      let open List.Let_syntax in
      let%bind n = divisors_from_factors d in
      let%map i = List.range 0 m ~stop:`inclusive in
      (p ** i) * n

let presents2 n =
  factors n |> divisors_from_factors
  |> List.filter ~f:(fun d -> n / d <= 50)
  |> sum |> ( * ) 11

let f2 s =
  let n = String.strip s |> Int.of_string in
  lowest_such_that ~f:(fun i -> presents2 i >= n)

let run () = Run.run ~name:"day20" ~f1 ~f2 Day20_input.data
