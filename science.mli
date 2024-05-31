module type Input = sig
  type t [@@deriving sexp_of]
end

module type Output = sig
  type t [@@deriving equal, sexp_of]
end

val try_ :
  ?bypass:bool ->
  control:('a -> 'b) ->
  experiment:('a -> 'b) ->
  (module Input with type t = 'a) ->
  (module Output with type t = 'b) ->
  'a ->
  'b
