(* lib/vector.ml *)
type vect = { mutable x : float; mutable y : float; mutable z : float }

let create_vect x y z = { x; y; z }

type vect_2d = { mutable x : float; mutable y : float }

let create_vect_2d x y = { x; y }
