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
