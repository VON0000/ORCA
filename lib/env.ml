(* lib/env.ml *)

(* nb de drones *)
let get_dim () =
  (* 打印提示信息 *)
  Printf.printf "input the number of drones: ";
  (* 刷新输出缓冲区，确保提示信息立即显示 *)
  flush stdout;
  (* 获取用户输入并解析为整数 *)
  let number =
    try
      let n = read_int () in
      Some n
    with Failure _ -> None
  in
  (* 根据解析结果输出相应的信息 *)
  match number with
  | Some n -> Printf.printf "the number of drones is: %d\n" n
  | None -> Printf.printf "invalid\n"
