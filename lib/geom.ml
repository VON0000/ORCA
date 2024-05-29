(*type vect={x: float;y: float} *)
type vect = { mutable x : float; mutable y : float }

(* p=position;s=speed;d=dest*)
type acft = { mutable p : vect; mutable s : vect; d : vect }

let create_vect x y = { x; y }
let create_acft p s d = { p; s; d }
let pi = acos (-1.)
let debug = ref false

(* operations de base *)
let mean a b = { x = (a.x +. b.x) /. 2.; y = (a.y +. b.y) /. 2. }
let diff a b = { x = a.x -. b.x; y = a.y -. b.y }
let scal a b = (a.x *. b.x) +. (a.y *. b.y)
let vect a b = (a.x *. b.y) -. (a.y *. b.x)

(* retourne la norme et l'angle de v*)
let normangle v = (sqrt (scal v v), atan2 v.y v.x)
let find_cap p d = atan2 (d.y -. p.y) (d.x -. p.x)
let opp v = { x = 0. -. v.x; y = 0. -. v.y }
let norm v = sqrt (scal v v)
let determ nax nbx nay nby = (nax *. nby) -. (nbx *. nay)

(* solve resoud un systeme de deux equations a deux inconnues*)

let epsilon = 0.0

exception Droites_confondues
exception No_solution

let normalize a =
  let d = sqrt (scal a a) in
  { x = a.x /. d; y = a.y /. d }

let solve pa na pb nb =
  let ca = scal na pa and cb = scal nb pb in
  let d = determ na.x nb.x na.y nb.y in
  if abs_float d < epsilon then
    let pr = { x = pb.x -. pa.x; y = pb.y -. pa.y } in
    if abs_float (vect pr na) < epsilon then (
      Printf.printf "pr=%f %f\n" pr.x pr.y;
      Printf.printf "na=%f %f\n" na.x na.y;
      flush stdout;
      raise Droites_confondues)
    else (
      Printf.printf "pr=%f %f\n" pr.x pr.y;
      Printf.printf "na=%f %f\n" na.x na.y;
      flush stdout;
      raise No_solution)
  else { x = determ ca cb na.y nb.y /. d; y = determ na.x nb.x ca cb /. d }

(* inside dit si un point est � l'int�rieur ou pas de la contrainte *)

let inside q p v =
  let pr = { x = q.x -. p.x; y = q.y -. p.y } in
  scal pr v >= 0.

(* termine le convexe (rajoute le point du d�but � la fin)*)

let complete cadre =
  match cadre with
  | [] -> []
  | deb :: tl -> (
      match List.rev tl with
      | [] -> cadre
      | fin :: _ -> if deb = fin then cadre else fin :: cadre)

(* intersect calcule l'intersection d'un convexe et d'un demi-plan*)

exception Vide

let intersect p v cadre =
  if !debug then (
    Printf.printf "p=%f %f\n" p.x p.y;
    flush stdout;
    Printf.printf "v=%f %f\n" v.x v.y;
    flush stdout);
  let resolve oq q p v = solve q { x = oq.y -. q.y; y = q.x -. oq.x } p v in
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

(*autres operations de base*)

let pvect p1 p0 p2 =
  ((p1.x -. p0.x) *. (p2.y -. p0.y)) -. ((p2.x -. p0.x) *. (p1.y -. p0.y))

let pscal p1 p0 p2 =
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

(* test l'egalit� de deux points *)
let equal_points pa pb = pa.x = pb.x && pa.y = pb.y

(* distance entre a et b *)
let dist2 a b =
  let dx = a.x -. b.x and dy = a.y -. b.y in
  (dx *. dx) +. (dy *. dy)

let dist a b = sqrt (dist2 a b)

(*couple (distance point c au segment ab, point de projection)*)
let proj_pt_seg c a b =
  if pscal b a b = 0. then (sqrt (pscal a c a), a)
  else if pscal b a c <= 0. then (sqrt (pscal a c a), a)
  else if pscal a b c <= 0. then (sqrt (pscal b c b), b)
  else
    let ab = sqrt (pscal b a b) in
    let u = { x = (b.x -. a.x) /. ab; y = (b.y -. a.y) /. ab } in
    let r = abs_float (pvect b a c) /. ab
    and s = abs_float (pscal b a c) /. ab in
    (r, { x = a.x +. (s *. u.x); y = a.y +. (s *. u.y) })

(*couple (projection point c sur le vecteur partant de a et de direction v )*)
let proj_pt_vec c a v =
  if scal v v = 0. then failwith "projection sur une droite sans direction";
  let ac = diff c a in
  let pv = vect ac v in
  let n, alpha = normangle v in
  let d = pv /. n in
  { x = d *. cos (alpha +. (pi /. 2.)); y = d *. sin (alpha +. (pi /. 2.)) }

(* croisement du segment a b et c d*)
let cross_segs a b c d =
  let ab = diff b a and ac = diff c a and ad = diff d a in
  let cd = diff d c and ca = diff a c and cb = diff b c in
  vect ab ac *. vect ab ad < 0. && vect cd ca *. vect cd cb < 0.

(* croisement du segment a b et convexe convex*)
let rec cross_segconv a b convex =
  match convex with
  | c :: d :: tl -> cross_segs a b c d || cross_segconv a b (d :: tl)
  | _ -> false

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

let projmax p convex =
  snd
    (List.fold_left
       (fun (d, v) q ->
         let t = pscal p q p in
         if t > d then (t, q) else (d, v))
       (0., p) convex)

(* tourne d'un angle a et multiplie par k le vecteur p*)
let rotate k p a =
  let pn, pa = normangle p in
  { x = k *. pn *. cos (pa +. a); y = k *. pn *. sin (pa +. a) }

(* donne les points extremes d'un convexe l vu du point a*)
let extremes a l =
  match l with
  | [] -> failwith "Erreur extremes"
  | hd :: _ ->
      let v0 = diff hd a in
      List.fold_left
        (fun (d, g, vd, vg) p ->
          let vc = diff p a in
          let pd = vect vd vc in
          if pd < 0. then (p, g, vc, vg)
          else
            let pg = vect vg vc in
            if pg > 0. then (d, p, vd, vc) else (d, g, vd, vg))
        (hd, hd, v0, v0) l

let box =
  List.rev
    [
      { x = -2.000000; y = -10.000000 };
      { x = -2.000000; y = 10.000000 };
      { x = 8.000000; y = 10.000000 };
      { x = 8.000000; y = -10.000000 };
      { x = -2.000000; y = -10.000000 };
    ]

let box2 =
  [
    { x = 2.575700; y = 7.995942 };
    { x = -0.675902; y = 10.325553 };
    { x = -7.402411; y = 0.936888 };
    { x = 2.465534; y = 7.842176 };
    { x = 2.575700; y = 7.995942 };
  ]

(*
let _ =
  let p0={x=0.;y=0.} in
  Printf.printf "%f %f\n\n" p0.x p0.y; flush stdout;
  if is_inside p0 box then failwith "is_inside OK"
*)

let outc = Unix.open_process_out "/usr/bin/gnuplot"

let test () =
  let fich = open_out "test" in
  List.iter (fun p -> Printf.fprintf fich "%f %f\n" p.x p.y) box;
  let p0 = { x = -20.; y = 11. } in
  let p1 = { x = -1.; y = 2. } in
  (if is_inside p1 box then failwith "is_inside OK"
     (*  if cross_segconv p0 p1 box then failwith "cross" *)
   else
     let p = projection p0 box in
     Printf.fprintf fich "\n%f %f\n" p0.x p0.y;
     Printf.fprintf fich "%f %f\n" p.x p.y;

     let d, g, _, _ = extremes p0 box in
     Printf.fprintf fich "\n%f %f\n" p0.x p0.y;
     Printf.fprintf fich "%f %f\n" d.x d.y;
     Printf.fprintf fich "\n%f %f\n" p0.x p0.y;
     Printf.fprintf fich "%f %f\n" g.x g.y);

  close_out fich;
  Printf.fprintf outc "plot [-20:20][-20:20] 'test' w lp \n";
  flush outc;

  Unix.select [] [] [] 100.
