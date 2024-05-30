(* lib/aircraft.ml *)
(* p=position; s=speed; d=dest *)
type acft = {
  mutable position : Vector.vect;
  mutable speed : Vector.vect;
  dest : Vector.vect;
}

let create_acft position speed dest = { position; speed; dest }
let get_position = failwith "not complete yet"
let get_speed = failwith "not complete yet"
let generate_dest = failwith "not complete yet"

let get_arft dim =
  (* 使用 Array.make 创建一个大小为 dim 的数组，每个元素都是相同的初始记录 *)
  let arfts =
    Array.make dim
      (create_acft
         (Vector.create_vect 0. 0. 0.)
         (Vector.create_vect 0. 0. 0.)
         (Vector.create_vect 0. 0. 0.))
  in

  (* 用于跟踪已生成的 dest 的列表 *)
  let existing_dests = ref [] in

  (* 迭代生成每个 acft 实例，并覆盖数组中的元素 *)
  for i = 0 to dim - 1 do
    let position = get_position () in
    let speed = get_speed () in
    let dest = generate_dest !existing_dests in
    existing_dests := dest :: !existing_dests;
    arfts.(i) <- create_acft position speed dest
  done;

  (* 返回生成的 acft 数组 *)
  arfts
