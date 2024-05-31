(* lib/aircraft.ml *)
(* p=position; s=speed; d=dest *)
type acft = {
  mutable position : Vector.vect;
  mutable speed : Vector.vect;
  dest : Vector.vect;
  mutable active : bool;
}

let create_acft position speed dest active = { position; speed; dest; active }

exception Exit

(* Helper function to judge the existence *)
let judge_exist exist_list position =
  (* flag -> true 间距合理
     flag -> false 间距不合理 *)
  let flag = ref true in
  try
    for i = 0 to List.length exist_list - 1 do
      if Geom.dist2 position (List.nth exist_list i) < Const.sep *. Const.norme2
      then (
        flag := false;
        raise Exit)
    done;
    !flag
  with Exit -> false

let get_position i existing_positions =
  let rec generate_position () =
    let position =
      Vector.create_vect
        (if i mod 2 = 0 then Const.radius *. (Random.float 2. -. 1.)
         else if i mod 4 = 1 then 0. -. Const.radius
         else Const.radius)
        (if i mod 2 = 1 then Const.radius *. (Random.float 2. -. 1.)
         else if i mod 4 = 2 then 0. -. Const.radius
         else Const.radius)
        0.
    in
    if judge_exist existing_positions position then position
    else generate_position ()
  in
  generate_position ()

let get_speed (position : Vector.vect) (dest : Vector.vect) =
  let angle = atan2 (dest.y -. position.y) (dest.x -. position.x) in
  (* { x = speed *. cos angle; y = speed *. sin angle } *)
  Vector.create_vect (Const.speed *. cos angle) (Const.speed *. sin angle) 0.

let generate_dest i existing_dests =
  let rec generate_dest () =
    let dest =
      Vector.create_vect
        (if i mod 2 = 0 then Const.radius *. (Random.float 2. -. 1.)
         else if i mod 4 = 1 then Const.radius
         else 0. -. Const.radius)
        (if i mod 2 = 1 then Const.radius *. (Random.float 2. -. 1.)
         else if i mod 4 = 2 then Const.radius
         else 0. -. Const.radius)
        0.
    in
    if judge_exist existing_dests dest then dest else generate_dest ()
  in
  generate_dest ()

(* 生成一个大小为 dim 的 acft 数组 *)

let get_arft dim =
  (* 使用 Array.make 创建一个大小为 dim 的数组，每个元素都是相同的初始记录 *)
  let arfts =
    Array.make dim
      (create_acft
         (Vector.create_vect 0. 0. 0.)
         (Vector.create_vect 0. 0. 0.)
         (Vector.create_vect 0. 0. 0.)
         true)
  in

  (* 用于跟踪已生成的 dest 的列表 *)
  let existing_dests = ref [] in
  let existing_positions = ref [] in

  (* 迭代生成每个 acft 实例，并覆盖数组中的元素 *)
  for i = 0 to dim - 1 do
    let position = get_position i !existing_positions in
    let dest = generate_dest i !existing_dests in
    let speed = get_speed position dest in
    existing_positions := position :: !existing_positions;
    existing_dests := dest :: !existing_dests;
    arfts.(i) <- create_acft position speed dest true
  done;

  (* 返回生成的 acft 数组 *)
  arfts
