(* p=position; s=speed; d=dest *)
open Geom

type acft = {
  position : Geom.t;
  speed : Geom.t;
  dest : Geom.t;
  active : bool;
  speedopt : Geom.t;
  route : Geom.t list;
}

let create_acft position speed dest active speedopt =
  { position; speed; dest; active; speedopt; route = [ position; dest ] }

exception Exit

(* Helper function to judge the existence *)
let judge_exist exist_list (position : Geom.t) =
  (* flag -> true 间距合理
     flag -> false 间距不合理 *)
  let flag = ref true in
  try
    for i = 0 to List.length exist_list - 1 do
      if
        Geom.dist2_2d position (List.nth exist_list i)
        < Const.sep *. Const.norme2
      then (
        flag := false;
        raise Exit)
    done;
    !flag
  with Exit -> false

let radius = 300. (*taille de la fenetre*)

let get_position i existing_positions =
  let rec generate_position () =
    let position =
      Geom.create_t
        (if i mod 2 = 0 then radius *. (Random.float 2. -. 1.)
         else if i mod 4 = 1 then 0. -. radius
         else radius)
        (if i mod 2 = 1 then radius *. (Random.float 2. -. 1.)
         else if i mod 4 = 2 then 0. -. radius
         else radius)
    in
    if judge_exist existing_positions position then position
    else generate_position ()
  in
  generate_position ()

let get_speed (position : Geom.t) (dest : Geom.t) =
  let angle = atan2 (dest.y -. position.y) (dest.x -. position.x) in
  Geom.create_t (Const.speed *. cos angle) (Const.speed *. sin angle)

let generate_dest i existing_dests =
  let rec generate_dest () =
    let dest =
      Geom.create_t
        (if i mod 2 = 0 then radius *. (Random.float 2. -. 1.)
         else if i mod 4 = 1 then radius
         else 0. -. radius)
        (if i mod 2 = 1 then radius *. (Random.float 2. -. 1.)
         else if i mod 4 = 2 then radius
         else 0. -. radius)
    in
    if judge_exist existing_dests dest then dest else generate_dest ()
  in
  generate_dest ()

let get_speedopt (position : Geom.t) (dest : Geom.t) =
  let cap =
    Geom.find_cap_2d
      (Geom.create_t position.x position.y)
      (Geom.create_t dest.x dest.y)
  in
  Geom.create_t (Const.speed *. cos cap) (Const.speed *. sin cap)

(* 生成一个大小为 dim 的 acft 数组 *)
let get_arft_lst dim =
  (* 使用 Array.make 创建一个大小为 dim 的数组，每个元素都是相同的初始记录 *)
  let arfts =
    Array.make dim
      (create_acft (Geom.create_t 0. 0.) (Geom.create_t 0. 0.)
         (Geom.create_t 0. 0.) true (Geom.create_t 0. 0.))
  in

  (* 用于跟踪已生成的 dest 的列表 *)
  let existing_dests = ref [] in
  let existing_positions = ref [] in

  (* 迭代生成每个 acft 实例，并覆盖数组中的元素 *)
  for i = 0 to dim - 1 do
    let position = get_position i !existing_positions in
    let dest = generate_dest i !existing_dests in
    let speed = get_speed position dest in
    let speedopt = get_speedopt position dest in
    existing_positions := position :: !existing_positions;
    existing_dests := dest :: !existing_dests;
    arfts.(i) <- create_acft position speed dest true speedopt
  done;

  (* 返回生成的 acft 数组 *)
  arfts

let read_acft acfts =
  for i = 0 to Array.length acfts do
    let _ = acfts.(i).position in
    let _ = acfts.(i).speed in
    let _ = acfts.(i).dest in
    let _ = acfts.(i).active in
    let _ = acfts.(i).speedopt in
    let _ = acfts.(i).route in
    ()
  done
