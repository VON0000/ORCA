(* operations de base *)
let mean_2d (a : Vector.vect_2d) (b : Vector.vect_2d) =
  ((a.x +. b.x) /. 2., (a.y +. b.y) /. 2.)

let diff_2d (a : Vector.vect_2d) (b : Vector.vect_2d) = (a.x -. b.x, a.y -. b.y)

(* 点积 *)
let scal_2d (a : Vector.vect_2d) (b : Vector.vect_2d) =
  (a.x *. b.x) +. (a.y *. b.y)

(* 叉乘 *)
let vect_2d (a : Vector.vect_2d) (b : Vector.vect_2d) =
  (a.x *. b.y) -. (a.y *. b.x)

(* retourne la norme et l'angle de v*)
let normangle_2d (v : Vector.vect_2d) = (sqrt (scal_2d v v), atan2 v.y v.x)

let dist2_2d (a : Vector.vect_2d) (b : Vector.vect_2d) =
  let dx = a.x -. b.x and dy = a.y -. b.y in
  (dx *. dx) +. (dy *. dy)

(* p到d的角度 *)
let find_cap_2d (p : Vector.vect_2d) (d : Vector.vect_2d) =
  atan2 (d.y -. p.y) (d.x -. p.x)

(* 反向 *)
let opp_2d (v : Vector.vect_2d) = Vector.create_vect_2d (-.v.x) (-.v.y)

let pvect_2d (p1 : Vector.vect_2d) (p0 : Vector.vect_2d) (p2 : Vector.vect_2d) =
  ((p1.x -. p0.x) *. (p2.y -. p0.y)) -. ((p2.x -. p0.x) *. (p1.y -. p0.y))