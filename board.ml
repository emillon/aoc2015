type 'a t = 'a array array

let fold t ~init ~f =
  Array.fold t ~init ~f:(fun acc ar -> Array.fold ar ~init:acc ~f)

let init ~dimx ~dimy ~f =
  Array.init dimy ~f:(fun y -> Array.init dimx ~f:(fun x -> f (x, y)))

let get t (x, y) = t.(y).(x)
let set t (x, y) v = t.(y).(x) <- v
