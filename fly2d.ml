(* lib/fly2d.ml *)
open Aircraft
open Geom
open Const

let fichmem = open_out "memory"
let is_stop dim fin = if fin < dim then true else false

(*mesure de la distance totale*)
let totdist = ref 0.

let move_all dim acfts flag_fin =
  for i = 0 to dim - 1 do
    let acft = List.nth acfts i in
    if acft.active then (
      Printf.fprintf fichmem "%f %f\n" acft.position.x acft.position.y;
      totdist := !totdist +. Geom.norm_2d acft.speed;
      Aircraft.move_one acft;
      if Geom.dist2_2d acft.position acft.dest < 4. *. Const.norme2 then (
        acft.active <- false;
        (* incr 用于递增一个引用类型的整数值 *)
        incr flag_fin);
      Printf.fprintf fichmem "%f %f\n\n" acft.position.x acft.position.y;
      flush fichmem)
  done

let get_constrants_entre_avions i acfts constraints =
  let local_acft = List.nth acfts i in
  for j = 0 to i - 1 do
    let ref_acft = List.nth acfts j in
    if ref_acft.active then ()
  done

let get_delta_speed previ local_acft d g =
  let delta_v_droite =
    Geom.projecton_point_to_vector previ local_acft.position
      (Geom.diff_2d d local_acft.position)
  in
  let angle_ext_position_d =
    Geom.angle_2d (Geom.diff_2d d local_acft.position)
  in
  let angle_ext_d =
    Geom.create_t
      (cos (angle_ext_position_d -. (Geom.pi /. 2.)))
      (sin (angle_ext_position_d -. (Geom.pi /. 2.)))
  in
  let delta_v_gauche =
    Geom.projecton_point_to_vector previ local_acft.position
      (Geom.diff_2d g local_acft.position)
  in
  let angle_ext_position_g =
    Geom.angle_2d (Geom.diff_2d g local_acft.position)
  in
  let angle_ext_g =
    Geom.create_t
      (cos (angle_ext_position_g +. (Geom.pi /. 2.)))
      (sin (angle_ext_position_g +. (Geom.pi /. 2.)))
  in
  if
    Geom.scal_2d delta_v_droite delta_v_droite
    < Geom.scal_2d delta_v_gauche delta_v_gauche
  then (delta_v_droite, angle_ext_d)
  else (delta_v_gauche, angle_ext_g)

let get_constrants_obstacle i acfts constraints =
  let local_acft = List.nth acfts i in
  let previ_opt_tau =
    Geom.create_t
      (local_acft.position.x +. (Const.tau *. local_acft.speedopt.x))
      (local_acft.position.y +. (Const.tau *. local_acft.speedopt.y))
  and previ =
    Geom.create_t
      (local_acft.position.x +. (Const.pas *. local_acft.speed.x))
      (local_acft.position.y +. (Const.pas *. local_acft.speed.y))
  and previ_tau =
    Geom.create_t
      (local_acft.position.x +. (Const.tau *. local_acft.speed.x))
      (local_acft.position.y +. (Const.tau *. local_acft.speed.y))
  in
  for j = 0 to Array.length Env.obstacle - 1 do
    if
      Geom.cross_segconv local_acft.position previ_opt_tau Env.obstacle.(j)
      || Geom.cross_segconv local_acft.position previ_tau Env.obstacle.(j)
    then
      let d, g = Geom.extremes local_acft.position Env.obstacle.(j) in
      let delta_v, angle_ext = get_delta_speed previ local_acft d g in
      constraints.(i) <- (delta_v, angle_ext, false) :: constraints.(i)
  done

let run =
  let flag_fin = ref 0 in
  let acfts = !Aircraft.acft_lst in
  while is_stop Const.dim !flag_fin do
    let constraints = Array.init dim (fun i -> []) in
    for i = 0 to dim - 1 do
      if (List.nth acfts i).active then (
        get_constrants_entre_avions i acfts constraints;
        get_constrants_obstacle i acfts constraints)
    done;

    move_all Const.dim acfts flag_fin
  done
