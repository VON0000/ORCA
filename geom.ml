type t = { x : float; y : float }

let create_t x y = { x; y }
let default_t = { x = 0.; y = 0. }

(* operations de base *)
let mean_2d (a : t) (b : t) = ((a.x +. b.x) /. 2., (a.y +. b.y) /. 2.)

(* b -> a 的向量 *)
let diff_2d (a : t) (b : t) = create_t (a.x -. b.x) (a.y -. b.y)

(* 点积 *)
(* > 0 -> a b 夹角小于90度
   < 0 -> a b 夹角大于90度
   = 0 -> a b 垂直 *)
let scal_2d (a : t) (b : t) = (a.x *. b.x) +. (a.y *. b.y)

(* 叉乘 *)
(* |a| * |b| * sin(theta) theta <- a 到 b 的角度*)
(* > 0 -> b 在 a 的逆时针方向（左侧）
   < 0 -> b 在 a 的顺时针方向（右侧）
   = 0 -> 平行或者共线 *)
let vectoriel_2d (a : t) (b : t) = (a.x *. b.y) -. (a.y *. b.x)

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

(* p0 -> p1 向量 和 p0 -> p2 向量 做外积
   结果大于零 夹角小于 180 p2位于 p0 -> p1 左边
   小于零 夹角大于 180 p2位于 p0 -> p1 右边 *)
let vectoriel_three_point_2d (p0 : t) (p1 : t) (p2 : t) =
  ((p1.x -. p0.x) *. (p2.y -. p0.y)) -. ((p2.x -. p0.x) *. (p1.y -. p0.y))

let normal_pass_o_2d (a : t) (b : t) = { x = a.y -. b.y; y = b.x -. a.x }

let is_inside a l =
  match l with
  | [] -> false
  | hd :: _ ->
      let rec is_in a l =
        match l with
        | b :: (c :: _ as ctl) ->
            vectoriel_three_point_2d b c a < 0. && is_in a ctl
        | [ b ] -> vectoriel_three_point_2d b hd a <= 0.
        | _ -> failwith "is_inside: unreachable"
      in
      is_in a l

(* croisement du segment a b et c d*)
(* 判断线段ab与cd是否相交 *)
(* 判断a,b(c,d)两点是否在线段cd(ab)两侧 *)
(* True -> 相交 *)
let cross_segs a b c d =
  let ab = diff_2d b a and ac = diff_2d c a and ad = diff_2d d a in
  let cd = diff_2d d c and ca = diff_2d a c and cb = diff_2d b c in
  vectoriel_2d ab ac *. vectoriel_2d ab ad < 0.
  && vectoriel_2d cd ca *. vectoriel_2d cd cb < 0.

(* croisement du segment a b et convexe convex*)
(* 线段ab是否与凸多边形convex有相交区域 *)
(* True -> 相交 *)
let rec cross_segconv a b convex =
  (* 模式匹配 *)
  match convex with
  (* convex是一个列表，c是convex中的第一个元素，d是第二个，tl表示剩下的元素 *)
  (* d :: tl 指的是从d开始的列表切片 *)
  | c :: d :: tl -> cross_segs a b c d || cross_segconv a b (d :: tl)
  | _ -> false

(* donne les points extremes d'un convexe l vu du point a*)
(* 用来找出从一点 a 观察一个凸多边形 l 时的两个极端点。 *)
(* 这里的“极端点”指的是多边形在从点 a 的视角看时的最左边和最右边的点。 *)
let extremes a l =
  match l with
  | [] -> failwith "Erreur extremes"
  | hd :: tl ->
      List.fold_left
        (* 整体逻辑为 对于判断最左边点 假设当前向量为最左边的 *)
        (* 通过迭代和叉乘判断下一个点是否在当前点的左边 *)
        (* d -> droite; g -> gauche *)
          (fun (d, g) p ->
          let ap = diff_2d p a in
          let ad = diff_2d d a and ag = diff_2d g a in
          let relative_ad_ap = vectoriel_2d ad ap in
          (* 找最左边的点 *)
          if relative_ad_ap < 0. then (p, g)
          else
            let relative_ag_ap = vectoriel_2d ag ap in
            (* 找最右边的点 *)
            if relative_ag_ap > 0. then (d, p) else (d, g))
        (hd, hd) l

let pi = acos (-1.)

(* 求的是c点到 过a点以v为向量的 直线的向量（方向指向c） *)
(* d 是c点到 过a点以v为向量的 直线的距离 *)
(* x,y 是 d 向量的坐标 *)
let projecton_point_to_vector c a v =
  if scal_2d v v = 0. then failwith "projection sur une droite sans direction";
  let ac = diff_2d c a in
  let alpha = angle_2d v in
  let d = vectoriel_2d ac v /. norm_2d v in
  if angle_2d ac > angle_2d v then
    { x = d *. cos (alpha +. (pi /. 2.)); y = d *. sin (alpha +. (pi /. 2.)) }
  else
    { x = d *. cos (alpha -. (pi /. 2.)); y = d *. sin (alpha -. (pi /. 2.)) }

let epsilon = 0.0

(* pa -> a 直线上任意一点
   na -> a 直线法向向量
   pb nb 同理 *)
exception Droites_confondues
exception No_solution

let intersection_point_of_two_line pa na pb nb =
  (* 判断两条直线是否平行或垂直 *)
  (* 判断两个法向向量是否方向相同 *)
  let d = vectoriel_2d na nb in
  if abs_float d < epsilon then
    let pr = diff_2d nb na in
    (* 判断两条直线是否共线 *)
    if abs_float (scal_2d pr na) < epsilon then (
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
    (* na.x + nb .y = na.x *. pa.x + na.y *. pa.y 为 a 的直线方程 *)
    (* 克莱姆法则 求解 a b 两方程的焦点 *)
    let ca = scal_2d na pa and cb = scal_2d nb pb in
    {
      x = vectoriel_2d (create_t ca na.y) (create_t cb nb.y) /. d;
      y = vectoriel_2d (create_t na.x ca) (create_t nb.x cb) /. d;
    }
