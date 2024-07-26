type t = { x : float; y : float }

let create_t x y = { x; y }
let default_t = { x = 0.; y = 0. }

(* operations de base *)
let mean_2d (a : t) (b : t) = ((a.x +. b.x) /. 2., (a.y +. b.y) /. 2.)
let diff_2d (a : t) (b : t) = create_t (a.x -. b.x) (a.y -. b.y)

(* 点积 *)
let scal_2d (a : t) (b : t) = (a.x *. b.x) +. (a.y *. b.y)

(* 叉乘 *)
let vect_2d (a : t) (b : t) = (a.x *. b.y) -. (a.y *. b.x)

(* retourne la norme et l'angle de a*)
let norm_2d (a : t) = sqrt (scal_2d a a)
let angle_2d (a : t) = atan2 a.y a.x

let heading_angle a b = atan2 (b.y -. a.y) (b.x -. a.x)

let dist2_2d (a : t) (b : t) =
  let dx = a.x -. b.x and dy = a.y -. b.y in
  (dx *. dx) +. (dy *. dy)

(* p到d的角度 *)
let find_cap_2d (p : t) (d : t) = atan2 (d.y -. p.y) (d.x -. p.x)

(* 反向 *)
let opp_2d (v : t) = create_t (-.v.x) (-.v.y)

let pvect_2d (p1 : t) (p0 : t) (p2 : t) =
  ((p1.x -. p0.x) *. (p2.y -. p0.y)) -. ((p2.x -. p0.x) *. (p1.y -. p0.y))

let determ_2d (na : t) (nb : t) = (na.x *. nb.y) -. (nb.x *. na.y)
