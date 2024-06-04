(*sortie d'affichage gnuplot*)
(* 进程间通信*)
let outc = Unix.open_process_out "/usr/bin/gnuplot"

let printpsb (arfts : Aircraft.acft array) (boites : Vector.vect_2d list array)
    =
  let fichp = open_out "points" in
  let fichb = open_out "boites" in
  let fichs = open_out "speeds" in
  let fichns = open_out "newspeeds" in
  for i = 0 to Const.dim - 1 do
    if arfts.(i).active then (
      Printf.fprintf fichp "%f %f\n" arfts.(i).position.x arfts.(i).position.y;
      flush fichp;
      Printf.fprintf fichs "%f %f\n%f %f\n\n" arfts.(i).position.x
        arfts.(i).position.y
        (arfts.(i).position.x +. (Const.coeff *. arfts.(i).speedopt.x))
        (arfts.(i).position.y +. (Const.coeff *. arfts.(i).speedopt.y));
      flush fichs;
      Printf.fprintf fichns "%f %f\n%f %f\n\n" arfts.(i).position.x
        arfts.(i).position.y
        (arfts.(i).position.x +. (Const.coeff *. arfts.(i).speed.x))
        (arfts.(i).position.y +. (Const.coeff *. arfts.(i).speed.y));
      flush fichns;
      List.iter
        (fun { x; y } ->
          Printf.fprintf fichb "%f %f\n"
            ((x *. coeff) +. arfts.(i).position.x)
            ((y *. coeff) +. arfts.(i).position.y))
        boites.(i);
      Printf.fprintf fichb "\n";
      flush fichb)
  done;
  close_out fichp;
  close_out fichb;
  close_out fichs;
  close_out fichns;
  (* 利用gnuplot绘图 *)
  Printf.fprintf outc
    "plot [-%d:%d][-%d:%d] 'obstacle' not w l lw 3,'points' not pt 7 ps \
     2,'speeds' w l,'newspeeds' w l,'routes' lc 4 w l,'boites' w l,'memory' \
     not w l lc 3\n"
    Const.dscreen Const.dscreen Const.dscreen Const.dscreen;
  flush outc;
  let _ = Unix.select [] [] [] Const.delai in
  ()
