(* p=position; s=speed; d=dest *)
open Geom

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

let get_position i existing_arfts =
  let existing_positions = List.map (fun x -> x#position) existing_arfts in
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

let get_dest i existing_arfts =
  let existing_dests = List.map (fun x -> x#dest) existing_arfts in
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

module Aircraft : AircraftSig = struct
  type t = {
    mutable position : Geom.t;
    mutable dest : Geom.t;
    mutable speed : Geom.t;
    mutable active : bool;
    mutable speedopt : Geom.t;
    mutable route : Geom.t list;
  }

  let create id exist_arfts =
    let position = get_position id exist_arfts in
    let dest = get_dest id exist_arfts in
    let speed = get_speed position dest in
    let speedopt = get_speedopt position dest in
    {
      position;
      dest;
      speed;
      active = true;
      speedopt;
      route = [position; dest];
    }

  let get_position a = a.position
  let get_dest a = a.dest
  let get_speed a = a.speed
  let get_speedopt a = a.speedopt
  let get_route a = a.route
  let is_active a = a.active
end

let get_arft_lst dim =
  let rec create_list n existing_arfts =
    if n <= 0 then existing_arfts
    else 
      let plane = Aircraft.create n existing_arfts in
      create_list (n - 1) (plane :: existing_arfts)
  in
  create_list dim []

let update_speedopt acft = get_speedopt acft.position acft.dest
