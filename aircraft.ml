(* p=position; s=speed; d=dest *)
open Geom

exception Exit

type t = {
  position : Geom.t;
  dest : Geom.t;
  speed : Geom.t;
  speedopt : Geom.t;
  route : Geom.t list;
  active : bool;
}

let radius = 300. (* taille de la fenêtre *)

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

let def_position id exist_arft =
  let existing_positions = List.map (fun x -> x.position) exist_arft in
  let rec generate_position () =
    let position =
      Geom.create_t
        (if id mod 2 = 0 then radius *. (Random.float 2. -. 1.)
         else if id mod 4 = 1 then 0. -. radius
         else radius)
        (if id mod 2 = 1 then radius *. (Random.float 2. -. 1.)
         else if id mod 4 = 2 then 0. -. radius
         else radius)
    in
    if judge_exist existing_positions position then position
    else generate_position ()
  in
  generate_position ()

let def_dest id exist_arft =
  let existing_dests = List.map (fun x -> x.dest) exist_arft in
  let rec generate_dest () =
    let dest =
      Geom.create_t
        (if id mod 2 = 0 then radius *. (Random.float 2. -. 1.)
         else if id mod 4 = 1 then radius
         else 0. -. radius)
        (if id mod 2 = 1 then radius *. (Random.float 2. -. 1.)
         else if id mod 4 = 2 then radius
         else 0. -. radius)
    in
    if judge_exist existing_dests dest then dest else generate_dest ()
  in
  generate_dest ()

let def_speed (position : Geom.t) (dest : Geom.t) =
  let angle = Geom.heading_angle position dest in
  Geom.create_t (Const.speed *. cos angle) (Const.speed *. sin angle)

let def_speedopt position dest =
  let cap = Geom.find_cap_2d position dest in
  Geom.create_t (Const.speed *. cos cap) (Const.speed *. sin cap)

let create id exist_arft =
  let position = def_position id exist_arft in
  let dest = def_dest id exist_arft in
  let speed = def_speed position dest in
  let speedopt = def_speedopt position dest in
  let arft =
    {
      position;
      dest;
      speed;
      active = true;
      speedopt;
      route = [ position; dest ];
    }
  in
  arft
