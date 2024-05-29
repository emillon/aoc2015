open Stdio

module Cmdliner_let_syntax = struct
  open Cmdliner.Term

  let ( let+ ) x k = const k $ x
end

let run ~name ~f1 ~f2 data =
  let info = Cmdliner.Cmd.info name in
  let term =
    let open Cmdliner_let_syntax in
    let+ part =
      let open Cmdliner.Arg in
      required
      & opt (some (enum [ ("1", `One); ("2", `Two) ])) None
      & info [ "part" ]
    in
    let f = match part with `One -> f1 | `Two -> f2 in
    printf "%d\n" (f data)
  in
  let cmd = Cmdliner.Cmd.v info term in
  Cmdliner.Cmd.eval cmd |> Stdlib.exit
