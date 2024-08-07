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

let get_constrants_entre_avions i acfts =
  let local_acft = List.nth acfts i in
  for j = 0 to i - 1 do
    let ref_acft = List.nth acfts j in
    if ref_acft.active then ()
  done

let get_constrants_obstacle i acfts =
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
      ()
  done

let run =
  let flag_fin = ref 0 in
  let acfts = Aircraft.acft_lst in
  while is_stop Const.dim !flag_fin do
    move_all Const.dim !acfts flag_fin
  done
