type 'a t = 'a array array

val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
val init : dimx:int -> dimy:int -> f:(Pos.t -> 'a) -> 'a t
val get : 'a t -> Pos.t -> 'a
val set : 'a t -> Pos.t -> 'a -> unit
