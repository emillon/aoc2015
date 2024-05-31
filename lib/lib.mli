open Base

module Cmdliner_let_syntax : sig
  type 'a t := 'a Cmdliner.Term.t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

val run :
  name:string -> f1:(string -> int) -> f2:(string -> int) -> string -> unit

module Dir : sig
  type t = N | S | E | W

  val of_char : char -> t
end

module Pos : sig
  type t = int * int [@@deriving hash, sexp]

  include Comparable.S with type t := t

  val shift : t -> Dir.t -> t
end

val number : int Angstrom.t
val sum : int list -> int
val permutations : 'a list -> 'a list list
val legs : 'a list -> ('a * 'a) list

module type Science_input = sig
  type t [@@deriving sexp_of]
end

module type Science_output = sig
  type t [@@deriving equal, sexp_of]
end

val science :
  ?bypass:bool ->
  control:('a -> 'b) ->
  experiment:('a -> 'b) ->
  (module Science_input with type t = 'a) ->
  (module Science_output with type t = 'b) ->
  'a ->
  'b
