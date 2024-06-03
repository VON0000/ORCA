let dim = Const.dim
let arfts = Aircraft.get_arft dim
let fin = ref 0
let totalt = ref 0
let constraints_vectu = Array.init dim (fun _ -> [])
let constraints_obstacle = Array.init dim (fun _ -> [])

let judge_with_arft i =
  for j = 0 to i - 1 do
    if arfts.(j).active then (
      incr totalt;
      match
        Action.vectu arfts.(i).position arfts.(i).speed arfts.(j).position
          arfts.(j).speed
      with
      | Some (pnv, qnv) ->
          if Geom.scal_2d pnv pnv > Const.epsilon then (
            if !Const.debug then (
              Printf.printf "%d %f %f %d %f %f %f %f\n" i arfts.(i).position.x
                arfts.(i).position.y j arfts.(j).position.x arfts.(j).position.y
                pnv.x pnv.y;
              flush stdout);
            constraints_vectu.(i) <- (pnv, qnv, true) :: constraints_vectu.(i);
            constraints_vectu.(j) <-
              (Geom.opp_2d pnv, Geom.opp_2d qnv, true) :: constraints_vectu.(j))
      | None -> ())
  done

let judge_with_obstacle i =
  let previopttau =
    Vector.create_vect_2d
      (arfts.(i).position.x +. (Const.tau *. arfts.(i).speedopt.x))
      (arfts.(i).position.y +. (Const.tau *. arfts.(i).speedopt.y))
  in
  let previ =
    Vector.create_vect_2d
      (arfts.(i).position.x +. (Const.pas *. arfts.(i).speed.x))
      (arfts.(i).position.y +. (Const.pas *. arfts.(i).speed.y))
  in
  let previtau =
    Vector.create_vect_2d
      (arfts.(i).position.x +. (Const.tau *. arfts.(i).speed.x))
      (arfts.(i).position.y +. (Const.tau *. arfts.(i).speed.y))
  in
  for j = 0 to Array.length Env.obstacle - 1 do
    if
      Geom.cross_segconv
        (Vector.vect_2_vect_2d arfts.(i).position)
        previopttau Env.obstacle.(j)
      || Geom.cross_segconv
           (Vector.vect_2_vect_2d arfts.(i).position)
           previtau Env.obstacle.(j)
    then (
      let d, g, vd, vg =
        Geom.extremes
          (Vector.vect_2_vect_2d arfts.(i).position)
          Env.obstacle.(j)
      in
      let fiche = open_out "extremes" in
      Printf.fprintf fiche "%f %f\n%f %f\n\n" arfts.(i).position.x
        arfts.(i).position.y d.x d.y;
      Printf.fprintf fiche "%f %f\n%f %f\n\n" arfts.(i).position.x
        arfts.(i).position.y g.x g.y;
      close_out fiche;
      let pd = Geom.proj_pt_vec previ arfts.(i).position vd in
      let _, angled = Geom.normangle_2d vd in
      let extd =
        Vector.create_vect_2d
          (cos (angled -. (Const.pi /. 2.)))
          (sin (angled -. (Const.pi /. 2.)))
      in
      let pg = Geom.proj_pt_vec previ arfts.(i).position vg in
      let _, angleg = Geom.normangle_2d vg in
      let extg =
        Vector.create_vect_2d
          (cos (angleg +. (Const.pi /. 2.)))
          (sin (angleg +. (Const.pi /. 2.)))
      in
      let p, ext =
        if Geom.scal_2d pd pd < Geom.scal_2d pg pg then (pd, extd)
        else (pg, extg)
      in
      constraints_obstacle.(i) <- (p, ext, false) :: constraints_obstacle.(i))
  done

let fly2d () =
  while !fin < dim do
    for i = 0 to dim - 1 do
      if arfts.(i).active then (
        judge_with_arft i;
        judge_with_obstacle i)
    done
  done
