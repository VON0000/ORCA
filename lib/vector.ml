(* lib/vector.ml *)
type vect = { mutable x : float; mutable y : float; mutable z : float }

let create_vect x y z = { x; y; z }
