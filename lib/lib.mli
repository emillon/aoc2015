module Cmdliner_let_syntax : sig
  type 'a t := 'a Cmdliner.Term.t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

val run :
  name:string -> f1:(string -> int) -> f2:(string -> int) -> string -> unit
