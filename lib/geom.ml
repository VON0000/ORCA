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

let determ_2d nax nbx nay nby = (nax *. nby) -. (nbx *. nay)

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

exception Droites_confondues
exception No_solution

let solve pa na pb nb =
  let ca = scal_2d na pa and cb = scal_2d nb pb in
  let d = determ_2d na.x nb.x na.y nb.y in
  if abs_float d < Const.epsilon then
    let pr = Vector.create_vect_2d (pb.x -. pa.x) (pb.y -. pa.y) in
    if abs_float (vect_2d pr na) < Const.epsilon then (
      Printf.printf "pr=%f %f\n" pr.x pr.y;
      Printf.printf "na=%f %f\n" na.x na.y;
      flush stdout;
      raise Droites_confondues)
    else (
      Printf.printf "pr=%f %f\n" pr.x pr.y;
      Printf.printf "na=%f %f\n" na.x na.y;
      flush stdout;
      raise No_solution)
  else
    Vector.create_vect_2d
      (determ_2d ca cb na.y nb.y /. d)
      (determ_2d na.x nb.x ca cb /. d)

(* inside dit si un point est � l'int�rieur ou pas de la contrainte *)

let inside (q : Vector.vect_2d) (p : Vector.vect_2d) v =
  let pr = Vector.create_vect_2d (q.x -. p.x) (q.y -. p.y) in
  scal_2d pr v >= 0.

let complete cadre =
  match cadre with
  | [] -> []
  | deb :: tl -> (
      match List.rev tl with
      | [] -> cadre
      | fin :: _ -> if deb = fin then cadre else fin :: cadre)

(* intersect calcule l'intersection d'un convexe et d'un demi-plan*)

exception Vide

let intersect (p : Vector.vect_2d) (v : Vector.vect_2d) cadre =
  if !Const.debug then (
    Printf.printf "p=%f %f\n" p.x p.y;
    flush stdout;
    Printf.printf "v=%f %f\n" v.x v.y;
    flush stdout);
  let resolve (oq : Vector.vect_2d) q p v =
    solve q { x = oq.y -. q.y; y = q.x -. oq.x } p v
  in
  let rec inter p v cadre sw oq l =
    match cadre with
    | [] -> ( match l with [] -> raise Vide | _ -> complete (List.rev l))
    | q :: tl ->
        if p = q then inter p v tl sw q l
        else
          let inn = inside q p v in
          if sw then
            if inn then inter p v tl sw q (q :: l)
            else
              let r = resolve oq q p v in
              inter p v tl false q (r :: l)
          else if inn then
            let r = resolve oq q p v in
            inter p v tl true q (q :: r :: l)
          else inter p v tl sw q l
  in
  match cadre with [] -> [] | hd :: tl -> inter p v tl (inside hd p v) hd []

let pvect (p1 : Vector.vect_2d) (p0 : Vector.vect_2d) (p2 : Vector.vect_2d) =
  ((p1.x -. p0.x) *. (p2.y -. p0.y)) -. ((p2.x -. p0.x) *. (p1.y -. p0.y))

let pscal (p1 : Vector.vect_2d) (p0 : Vector.vect_2d) (p2 : Vector.vect_2d) =
  ((p1.x -. p0.x) *. (p2.x -. p0.x)) +. ((p1.y -. p0.y) *. (p2.y -. p0.y))

(* is_inside=true si un point (x,y) est dans le convexe l qui est decrit dans
   le sens INVERSE des aiguilles d'une montre*)
let is_inside a l =
  match l with
  | [] -> false
  | hd :: _ ->
      let rec is_in a l =
        match l with
        | b :: (c :: _ as ctl) -> pvect b c a < 0. && is_in a ctl
        | [ b ] -> pvect b hd a <= 0.
        | _ -> failwith "is_inside: unreachable"
      in
      is_in a l

(*couple (distance point c au segment ab, point de projection)*)
let proj_pt_seg c a b =
  if pscal b a b = 0. then (sqrt (pscal a c a), a)
  else if pscal b a c <= 0. then (sqrt (pscal a c a), a)
  else if pscal a b c <= 0. then (sqrt (pscal b c b), b)
  else
    let ab = sqrt (pscal b a b) in
    let u = Vector.create_vect_2d ((b.x -. a.x) /. ab) ((b.y -. a.y) /. ab) in
    let r = abs_float (pvect b a c) /. ab
    and s = abs_float (pscal b a c) /. ab in
    (r, { x = a.x +. (s *. u.x); y = a.y +. (s *. u.y) })

(* projection d'un vecteur sur un convexe *)
let projection p convex =
  let rec rec_project p convex oq r rp =
    match convex with
    | [] -> rp
    | q :: tl ->
        let s, sp = proj_pt_seg p oq q in
        if s < r then rec_project p tl q s sp else rec_project p tl q r rp
  in
  match convex with
  | [] -> failwith "convex vide"
  | oq :: _ -> rec_project p convex oq (1. /. 0.) oq

(* projection d'une vitesse sur une boite*)
let project speed targetbox =
  if is_inside speed targetbox then speed else projection speed targetbox

(* tourne d'un angle a et multiplie par k le vecteur p*)
let rotate k p a =
  let pn, pa = normangle_2d p in
  Vector.create_vect_2d (k *. pn *. cos (pa +. a)) (k *. pn *. sin (pa +. a))