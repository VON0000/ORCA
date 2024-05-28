let () =
  let fin=ref 0 in
  let t=ref 0 in
  let totalt=ref 0 in
  let fichroutes=open_out "routes" in
  for i=0 to dim-1 do
    List.iter (fun p -> Printf.fprintf fichroutes "%f %f\n" p.x p.y) routes.(i);
    Printf.fprintf fichroutes "\n";
  done;
  close_out fichroutes;
  let fichobstacle=open_out "obstacle" in
  for i=0 to Array.length obstacle -1 do
    List.iter (fun p -> Printf.fprintf fichobstacle "%f %f\n" p.x p.y) obstacle.(i);
    Printf.fprintf fichobstacle "\n";
  done;
  close_out fichobstacle;

  let fichmem=open_out "memory" in
  while (!fin<dim) do 
    (* calcule la vitesse optimale qui va vers la destination*)
    let speedopt=Array.init dim (fun i -> 
      let cap =find_cap points.(i) dests.(i) in
      {x=speed*.cos cap;y=speed*.sin cap}) in
    (* calcule un tableau liste des contraintes pour chaque avion*)
    let oldspeeds=Array.init dim (fun i -> speeds.(i)) in
    let constraints=Array.init dim (fun _ -> []) in
    for i=0 to dim-1 do
      if active.(i) then 
	      begin
        (*gestion des contraintes entre avions*)
	        for j=0 to i-1 do
	          if active.(j) then
	            begin
                incr totalt;
                match vectu points.(i) speeds.(i) points.(j) speeds.(j) tau with
                  Some (pnv,qnv) ->
                    begin
                      if scal pnv pnv > epsilon then 
                        begin
                          if !debug then 
                            (Printf.printf "%d %f %f %d %f %f %f %f\n" 
                            i points.(i).x points.(i).y 
                            j points.(j).x points.(j).y 
                            pnv.x pnv.y; flush stdout);
                            constraints.(i) <-(pnv,qnv,true)::constraints.(i);
                            constraints.(j) <-(opp pnv,opp qnv,true)::constraints.(j); 
                        end
                    end
                  | None -> ()
              end
	        done;
    (*gestion des obstacles*)
    let previopttau ={x=points.(i).x +.tau*.speedopt.(i).x; y=points.(i).y +. tau*.speedopt.(i).y} in
    let previ ={x=points.(i).x +. pas*.speeds.(i).x; y=points.(i).y +. pas*.speeds.(i).y} in
    let previtau ={x=points.(i).x +. tau*.speeds.(i).x; y=points.(i).y +. tau*.speeds.(i).y} in
    for j=0 to Array.length obstacle -1 do
      if cross_segconv points.(i) previopttau obstacle.(j) ||
            cross_segconv points.(i) previtau obstacle.(j) 
        then 
          begin
            let (d,g,vd,vg)=extremes points.(i) obstacle.(j) in
            let fiche=open_out "extremes" in
            Printf.fprintf fiche "%f %f\n%f %f\n\n" points.(i).x points.(i).y d.x d.y;
            Printf.fprintf fiche "%f %f\n%f %f\n\n" points.(i).x points.(i).y g.x g.y;
            close_out fiche;
            let pd=proj_pt_vec previ points.(i) vd  in
            let (_nd,angled)=normangle vd in
            let extd={x=cos (angled-.pi/.2.);y=sin (angled-.pi/.2.)} in
            let pg=proj_pt_vec previ points.(i) vg  in
            let (_ng,angleg)=normangle vg in
            let extg={x=cos (angleg+.pi/.2.);y=sin (angleg+.pi/.2.)} in
            let p,ext= if scal pd pd < scal pg pg then pd,extd else pg,extg  in
            constraints.(i) <-(p,ext,false)::constraints.(i); 
                
          end;
        done;
	  end
  done;

  let boites=Array.init dim (fun _ -> []) in
  for i=0 to dim-1 do
    let initbox=speedbox points.(i) speeds.(i) speedopt.(i) sizelong sizelat nspeeds.(i) in
    let targetbox= cut i initbox speeds.(i) constraints.(i) in

    (*projette la vitesse opt sur la boite des vitesses admissibles*)
    let newspeed=
      let ns=project i speedopt.(i) targetbox in
      if sqrt (scal ns ns) > speed/.5. then ns
      else
        let ns=project i (rotate 1. speeds.(i) (0.)) targetbox in ns
    in          
    if !debug then (Printf.printf "newspeed=%f %f\n" newspeed.x newspeed.y; flush stdout);
    oldspeeds.(i) <- speeds.(i);
    speeds.(i) <- newspeed;
    boites.(i) <- targetbox;
  done;
  if !t mod step=0 then
    printpsb false points speedopt speeds boites;
  move points speeds obstacle fichmem fin;
  incr t;
done;
Printf.printf "STEP=%d TOTALT=%d TOTALDIST=%f Distance Min=%f\n" !t !totalt !totdist !mini;flush stdout;
let _=Unix.select [] [] [] 10. in flush outc;

    

