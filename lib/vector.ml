(* lib/vector.ml *)
type vect = { mutable x : float; mutable y : float; mutable z : float }

let create_vect x y z = { x; y; z }

type vect_2d = { mutable x : float; mutable y : float }

let create_vect_2d x y = { x; y }
let vect_2_vect_2d (v : vect) = { x = v.x; y = v.y }
let vect_2_vect_2ds (vs : vect list) = List.map vect_2_vect_2d vs