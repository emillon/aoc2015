type t = int * int [@@deriving hash, sexp]

include Comparable.S with type t := t

val shift : t -> Dir.t -> t

val neighbours8 : t -> t list
