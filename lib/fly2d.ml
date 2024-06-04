let dim = Const.dim
let arfts = Aircraft.get_arft dim
let fin = ref 0
let totalt = ref 0
let constraints_aircraft = Array.init dim (fun _ -> [])
let constraints_obstacle = Array.init dim (fun _ -> [])
let oldspeeds = Array.init dim (fun i -> arfts.(i).speed)

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
            constraints_aircraft.(i) <-
              (pnv, qnv, true) :: constraints_aircraft.(i);
            constraints_aircraft.(j) <-
              (Geom.opp_2d pnv, Geom.opp_2d qnv, true)
              :: constraints_aircraft.(j))
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
    then
      let _, _, vd, vg =
        Geom.extremes
          (Vector.vect_2_vect_2d arfts.(i).position)
          Env.obstacle.(j)
      in
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
      constraints_obstacle.(i) <- (p, ext, false) :: constraints_obstacle.(i)
  done

(*boite des vitesses possibles version circulaire à facettes*)
let speedbox speed =
  let _d, angle = Geom.normangle_2d (Vector.vect_2_vect_2d speed) in
  (*  let nspeed={x=ns*.speed.x/.d;y=ns*.speed.y/.d} in *)
  let nbf = float Const.nb in
  let box =
    Array.init (Const.nb + 1) (fun i ->
        let na = angle +. (float i *. 2. *. Const.pi /. nbf) in
        Vector.create_vect_2d
          (Const.sizelong *. cos na)
          (Const.sizelong *. sin na))
  in
  (*  if is_inside {x=speed.x;y=speed.y} (Array.to_list box) then Printf.printf "ISINSIDE\n";flush stdout; *)
  Array.to_list box

exception Fin
exception Echec
exception Vide

(*intersection boite des vitesses et contraintes*)
let choose_speed i (initbox : Vector.vect_2d list)
    (constraints : (Vector.vect_2d * Vector.vect_2d * bool) list) =
  let box = ref initbox in
  let delta = ref (-0.5) in
  (try
     while true do
       box := initbox;
       try
         List.iter
           (fun ((nv : Vector.vect_2d), (qv : Vector.vect_2d), tf) ->
             let r = sqrt (Geom.scal_2d nv nv) in
             let uv = Vector.create_vect_2d (qv.x /. r) (qv.y /. r) in
             let p =
               Vector.create_vect_2d
                 (arfts.(i).speed.x +. nv.x -. if tf then !delta *. uv.x else 0.)
                 (arfts.(i).speed.y +. nv.y -. if tf then !delta *. uv.y else 0.)
             in
             box := Geom.intersect p qv !box)
           constraints;
         raise Fin
       with Vide -> ()
     done
   with Fin -> ());
  !box

let get_available_speed () =
  let boites = Array.make dim [] in
  for i = 0 to dim - 1 do
    let initbox = speedbox arfts.(i).speed in
    let targetbox_arft = choose_speed i initbox constraints_aircraft.(i) in
    let targetbox_obst = choose_speed i initbox constraints_obstacle.(i) in
    let targetbox = Vector.intersect targetbox_arft targetbox_obst in

    (*projette la vitesse opt sur la boite des vitesses admissibles*)
    let newspeed =
      let ns =
        Geom.project (Vector.vect_2_vect_2d arfts.(i).speedopt) targetbox
      in
      if sqrt (Geom.scal_2d ns ns) > Const.speed /. 5. then ns
      else
        Geom.project
          (Vector.vect_2_vect_2d arfts.(i).speed)
          (*Geom.rotate 1. (Vector.vect_2_vect_2d arfts.(i).speed) 0.*)
          targetbox
    in
    oldspeeds.(i) <- arfts.(i).speed;
    arfts.(i).speed.x <- newspeed.x;
    arfts.(i).speed.y <- newspeed.y;
    boites.(i) <- targetbox
  done;
  boites

(*routes prévues*)
(* 目标点和坐标的映射关系 *)
let routes = Array.init dim (fun i -> [ arfts.(i).position; arfts.(i).dest ])

let fly2d () =
  let fichroutes = open_out "routes" in
  for i = 0 to dim - 1 do
    List.iter (fun p -> Printf.fprintf fichroutes "%f %f\n" p.x p.y) routes.(i);
    Printf.fprintf fichroutes "\n"
  done;
  close_out fichroutes;
  let fichobstacle = open_out "obstacle" in
  for i = 0 to Array.length Env.obstacle - 1 do
    List.iter
      (fun p -> Printf.fprintf fichobstacle "%f %f\n" p.x p.y)
      Env.obstacle.(i);
    Printf.fprintf fichobstacle "\n"
  done;
  close_out fichobstacle;

  let fichmem = open_out "memory" in
  while !fin < dim do
    for i = 0 to dim - 1 do
      if arfts.(i).active then (
        judge_with_arft i;
        judge_with_obstacle i;
        let boites = get_available_speed () in
        Plot.printpsb arfts boites)
    done
  done
