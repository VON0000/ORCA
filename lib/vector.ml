(* lib/vector.ml *)
type vect = { mutable x : float; mutable y : float; mutable z : float }

let create_vect x y z = { x; y; z }

type vect_2d = { mutable x : float; mutable y : float }

let create_vect_2d x y = { x; y }
let vect_2_vect_2d (v : vect) = { x = v.x; y = v.y }
let vect_2_vect_2ds (vs : vect list) = List.map vect_2_vect_2d vs
let vect_2d_equal v1 v2 = v1.x = v2.x && v1.y = v2.y

let intersect lst1 lst2 =
  List.filter (fun v -> List.exists (vect_2d_equal v) lst2) lst1