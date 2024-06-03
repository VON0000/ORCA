(* 定义一个无穷大的数 *)
let mini = ref (1. /. 0.)

let vectu (pa : Vector.vect) (sa : Vector.vect) (pb : Vector.vect)
    (sb : Vector.vect) =
  Const.debug := false;
  (* a,b相对位置 *)
  let pr_2d = Vector.create_vect_2d (pb.x -. pa.x) (pb.y -. pa.y) in
  let npr, npa = Geom.normangle_2d pr_2d in
  mini := min !mini npr;
  (*  *)
  let prbis =
    Vector.create_vect_2d (pr_2d.x /. Const.tau) (pr_2d.y /. Const.tau)
  in
  let nprbis = sqrt (Geom.scal_2d prbis prbis) in
  (* a,b相对速度 *)
  let vr_2d = Vector.create_vect_2d (sa.x -. sb.x) (sa.y -. sb.y) in
  let nvr, nva = Geom.normangle_2d vr_2d in

  let vrbis = Vector.create_vect_2d (vr_2d.x -. prbis.x) (vr_2d.y -. prbis.y) in
  let nvrbis = sqrt (Geom.scal_2d vrbis vrbis) in

  let region_neutre (vrbis : Vector.vect_2d) (nvrbis : float) =
    let rayon = (2. *. Const.norme /. Const.tau) -. nvrbis in
    let res =
      Vector.create_vect_2d
        (rayon *. vrbis.x /. nvrbis /. 2.)
        (rayon *. vrbis.y /. nvrbis /. 2.)
    in

    (*    Printf.printf "NEUTRE";*)
    if rayon >= 0. then Some (res, res) else Some (res, Geom.opp_2d res)
  in

  (*  (res,opp res)  in *)
  if npr < 2. *. Const.norme then (
    (* conflit entre avions *)
    Printf.printf "%f %f %f %f\n" pa.x pa.y pb.x pb.y;
    flush stdout;
    (*      while true do () done; *)
    failwith "npr<2*norme"
    (*      region_neutre vrbis nvrbis  *))
  else
    let teta = asin (2. *. Const.norme /. npr) in
    let npa1 = npa -. teta and npa2 = npa +. teta in
    let p1 = Vector.create_vect_2d (nprbis *. cos npa1) (nprbis *. sin npa1)
    and p2 = Vector.create_vect_2d (nprbis *. cos npa2) (nprbis *. sin npa2) in

    if
      Geom.pvect_2d p1 prbis vr_2d > 0. && Geom.pvect_2d vr_2d prbis pr_2d >= 0.
    then (
      if !Const.debug then (
        Printf.printf "SECTEUR 1\n";
        flush stdout);
      let alpha = nva -. npa1 in
      let nvrsa = nvr *. sin alpha in
      let abs_nvrsa = abs_float nvrsa in
      let u =
        Vector.create_vect_2d
          (cos (npa1 -. (Const.pi /. 2.)) /. 2.)
          (sin (npa1 -. (Const.pi /. 2.)) /. 2.)
      in
      Some
        ( Vector.create_vect_2d (nvrsa *. u.x) (nvrsa *. u.y),
          Vector.create_vect_2d (abs_nvrsa *. u.x) (abs_nvrsa *. u.y) ))
    else if
      Geom.pvect_2d pr_2d prbis vr_2d > 0. && Geom.pvect_2d vr_2d prbis p2 >= 0.
    then (
      if !Const.debug then (
        Printf.printf "SECTEUR 2\n";
        flush stdout);
      let alpha = npa2 -. nva in
      let nvrsa = nvr *. sin alpha in
      let abs_nvrsa = abs_float nvrsa in
      let u =
        Vector.create_vect_2d
          (cos (npa2 +. (Const.pi /. 2.)) /. 2.)
          (sin (npa2 +. (Const.pi /. 2.)) /. 2.)
      in
      Some
        ( Vector.create_vect_2d (nvrsa *. u.x) (nvrsa *. u.y),
          Vector.create_vect_2d (abs_nvrsa *. u.x) (abs_nvrsa *. u.y) ))
    else region_neutre vrbis nvrbis
