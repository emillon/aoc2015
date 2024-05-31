open Cmdliner.Term

let ( let+ ) x k = const k $ x
