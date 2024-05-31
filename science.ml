open Base

module type Input = sig
  type t [@@deriving sexp_of]
end

module type Output = sig
  type t [@@deriving equal, sexp_of]
end

let try_ (type input output) ?(bypass = false) ~control ~experiment
    (module Input : Input with type t = input)
    (module Output : Output with type t = output) x =
  let control = control x in
  if bypass then control
  else
    let exp = experiment x in
    if not (Output.equal control exp) then
      raise_s [%message (x : Input.t) (control : Output.t) (exp : Output.t)];
    control
