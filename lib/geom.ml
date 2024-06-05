type t = { mutable x : float; mutable y : float }

let create_t x y = { x; y }

(* operations de base *)
let mean_2d (a : t) (b : t) = ((a.x +. b.x) /. 2., (a.y +. b.y) /. 2.)
let diff_2d (a : t) (b : t) = create_t (a.x -. b.x) (a.y -. b.y)

(* 点积 *)
let scal_2d (a : t) (b : t) = (a.x *. b.x) +. (a.y *. b.y)

(* 叉乘 *)
let vect_2d (a : t) (b : t) = (a.x *. b.y) -. (a.y *. b.x)

(* retourne la norme et l'angle de v*)
let norm_2d (v : t) = sqrt (scal_2d v v)
let angle_2d (v : t) = atan2 v.y v.x

let dist2_2d (a : t) (b : t) =
  let dx = a.x -. b.x and dy = a.y -. b.y in
  (dx *. dx) +. (dy *. dy)

(* p到d的角度 *)
let find_cap_2d (p : t) (d : t) = atan2 (d.y -. p.y) (d.x -. p.x)

(* 反向 *)
let opp_2d (v : t) = create_t (-.v.x) (-.v.y)

let pvect_2d (p1 : t) (p0 : t) (p2 : t) =
  ((p1.x -. p0.x) *. (p2.y -. p0.y)) -. ((p2.x -. p0.x) *. (p1.y -. p0.y))

let determ_2d nax nbx nay nby = (nax *. nby) -. (nbx *. nay)
