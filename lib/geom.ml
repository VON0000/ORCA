(* operations de base *)
let mean_2d (a : Vector.vect_2d) (b : Vector.vect_2d) =
  ((a.x +. b.x) /. 2., (a.y +. b.y) /. 2.)

let diff_2d (a : Vector.vect_2d) (b : Vector.vect_2d) =
  Vector.create_vect_2d (a.x -. b.x) (a.y -. b.y)

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

(* croisement du segment a b et c d*)
(* 判断线段ab与cd是否相交 *)
(* 判断a,b(c,d)两点是否在线段cd(ab)两侧 *)
let cross_segs a b c d =
  let ab = diff_2d b a and ac = diff_2d c a and ad = diff_2d d a in
  let cd = diff_2d d c and ca = diff_2d a c and cb = diff_2d b c in
  vect_2d ab ac *. vect_2d ab ad < 0. && vect_2d cd ca *. vect_2d cd cb < 0.

(* croisement du segment a b et convexe convex*)
(* 线段ab是否与凸多边形convex有相交区域 *)
let rec cross_segconv a b convex =
  (* 模式匹配 *)
  match convex with
  (* convex是一个列表，c是convex中的第一个元素，d是第二个，tl表示剩下的元素 *)
  (* d :: tl 指的是从d开始的列表切片 *)
  | c :: d :: tl ->
      cross_segs a b (Vector.vect_2_vect_2d c) (Vector.vect_2_vect_2d d)
      || cross_segconv a b (d :: tl)
  | _ -> false

(* donne les points extremes d'un convexe l vu du point a*)
(* 用来找出从一点 a 观察一个凸多边形 l 时的两个极端点。 *)
(* 这里的“极端点”指的是多边形在从点 a 的视角看时的最左边和最右边的点。 *)
let extremes a l =
  match l with
  | [] -> failwith "Erreur extremes"
  | hd :: _ ->
      let v0 = diff_2d (Vector.vect_2_vect_2d hd) a in
      List.fold_left
        (* 整体逻辑为 对于判断最左边点 假设当前向量为最左边的 *)
        (* 通过迭代和叉乘判断下一个点是否在当前点的左边 *)
          (fun (d, g, vd, vg) p ->
          let vc = diff_2d p a in
          let pd = vect_2d vd vc in
          if pd < 0. then (p, g, vc, vg)
          else
            let pg = vect_2d vg vc in
            if pg > 0. then (d, p, vd, vc) else (d, g, vd, vg))
        (Vector.vect_2_vect_2d hd, Vector.vect_2_vect_2d hd, v0, v0)
        (Vector.vect_2_vect_2ds l)

(*couple (projection point c sur le vecteur partant de a et de direction v )*)
(* d 是 ac 向量到 v 向量的距离 d = (vect ac v) / norm v *)
(* x,y 是 d 向量的坐标 *)
let proj_pt_vec c a v =
  if scal_2d v v = 0. then failwith "projection sur une droite sans direction";
  let ac = diff_2d c (Vector.vect_2_vect_2d a) in
  let pv = vect_2d ac v in
  let n, alpha = normangle_2d v in
  let d = pv /. n in
  Vector.create_vect_2d
    (d *. cos (alpha +. (Const.pi /. 2.)))
    (d *. sin (alpha +. (Const.pi /. 2.)))
