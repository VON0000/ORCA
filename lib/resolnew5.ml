open geom
(*
let seed=truncate (Unix.time ())
let _ = Random.init seed
*)
  
(* liste des constantes du problème*)
let dim=10 (*nb de drones*)
let nb=20 (*nombre de facettes de la boite contenant les vitesses*)
let norme=2.5 (*demi norme de separation*)
let norme2=4.*.norme*.norme  (*norme de separation au carré*)
let pas=1. (*pas de temps*)
let radius=300. (*taille de la fenetre*) 
let dscreen=50+truncate radius (*taille de la fenetre étendue*)
let speed=1. (* vitesse *)
let sizelong=1.*.pas (*manoeuvrabilité longitudinale*)
let sizelat=0.1*.pas (*manoeuvrabilité latérale*)
let pi= acos (-1.) 
let delai=0.5 (*delai pour ralentir l'affichage*)
let tau=60. (*horizon en secondes*)
let step=10 (*pas de temps affichage *)
let sep=5. (*nombre de normes de separations pour créer les orig-dest*)
let coeff=50. (*zoom d'affichage des rayons des vitesse*)

let obstacle=
[|  List.rev  [{x= 50.000000;y= -100.000000};
             {x= 50.000000;y=  200.000000};
             {x=  100.000000;y=  200.000000};
             {x=  100.000000;y= -100.000000};
             {x= 50.000000;y= -100.000000}];
      List.rev  [{x= -150.000000;y= -120.000000};
             {x= -150.000000;y=  10.000000};
             {x=  10.000000;y=  10.000000};
             {x=  10.000000;y= -120.000000};
             {x= -150.000000;y= -120.000000}];
      List.rev  [{x= -100.000000;y= 50.000000};
             {x= -100.000000;y=  100.000000};
             {x=  -50.000000;y=  100.000000};
             {x=  -50.000000;y=  50.000000};
             {x= -100.000000;y= 50.000000}]|]

exception Good

(* position courante des drones*)        
let points= 
  let tab=
    Array.init dim (fun _ -> {x=0.;y=0.}) in
  for i = 0 to dim-1 do
    try
      while true do
	tab.(i) <-{x=
	    if i mod 2=0 then radius*.(Random.float 2. -.1.) 
	    else if i mod 4=1 then (0.-.radius) else radius;
		   y=
	    if i mod 2=1 then radius*.(Random.float 2. -.1.) 
	    else if i mod 4=2 then (0.-.radius) else radius};     
	  (try
	     for j=0 to i-1 do
	       if (dist2 tab.(i) tab.(j)) < (sep*.norme2) then
		 raise Exit;
	     done;
	     raise Good;
	   with Exit -> ());
      done;
    with Good -> ()
  done;
  tab

(*destinations des drones*)  
let dests= 
  let tab=
    Array.init dim (fun _ -> {x=0.;y=0.}) in
  for i = 0 to dim-1 do
    try
      while true do
	tab.(i) <-{x=
	    if i mod 2=0 then radius*.(Random.float 2. -.1.) 
	    else if i mod 4=1 then (radius) else (0.-.radius);
		   y=
	    if i mod 2=1 then radius*.(Random.float 2. -.1.) 
	    else if i mod 4=2 then (radius) else (0.-.radius)};     
	  (try
	     for j=0 to i-1 do
	       if (dist2 tab.(i) tab.(j)) < (sep*.norme2) then
		 raise Exit;
	     done;
	     raise Good;
	   with Exit -> ());
      done;
    with Good -> ()
  done;
  tab

(*routes prévues*)  
let routes=Array.init dim (fun i -> [points.(i);dests.(i)])

(*vecteurs vitesses*)         
let speeds=Array.init dim (fun i -> 
               let angle=atan2 (dests.(i).y-.points.(i).y)
                           (dests.(i).x-.points.(i).x) in
               {x= speed*.(cos (angle));y=speed*.(sin (angle))})

(*normes des vitesses*)         
let nspeeds=Array.map norm speeds

(*drones actifs, cad qui n'ont pas atteint leur dest*)          
let active=Array.init dim (fun _ -> true)

(*sortie d'affichage gnuplot*)         
let outc = Unix.open_process_out "/usr/bin/gnuplot" 

(*affichage gnuplot*)         
let printpsb _debug (*conflit*) points speeds newspeeds boites=
  let fichp=open_out "points" in
  let fichb=open_out "boites" in
  let fichs=open_out "speeds" in
  let fichns=open_out "newspeeds" in
  for i=0 to dim-1 do
    if active.(i) then 
      begin
        (Printf.fprintf fichp "%f %f\n" points.(i).x points.(i).y; flush fichp);
        Printf.fprintf fichs "%f %f\n%f %f\n\n"
          points.(i).x points.(i).y
          (points.(i).x+.coeff*.speeds.(i).x) (points.(i).y+.coeff*.speeds.(i).y);
        flush fichs;
        Printf.fprintf fichns "%f %f\n%f %f\n\n"
          points.(i).x points.(i).y
          (points.(i).x+.coeff*.newspeeds.(i).x) (points.(i).y+.coeff*.newspeeds.(i).y);
        flush fichns;
        List.iter (fun {x=x;y=y} ->
            Printf.fprintf fichb "%f %f\n"
              (x*.coeff+.points.(i).x) (y*.coeff+.points.(i).y)) boites.(i);
        Printf.fprintf fichb "\n";
        flush fichb;
      end
  done;
  close_out fichp;
  close_out fichb;
  close_out fichs;
  close_out fichns;
     Printf.fprintf outc "plot [-%d:%d][-%d:%d] 'obstacle' not w l lw 3,'points' not pt 7 ps 2,'speeds' w l,'newspeeds' w l,'routes' lc 4 w l,'boites' w l,'memory' not w l lc 3\n"
       dscreen dscreen dscreen dscreen;     
   flush outc;
     let _ = Unix.select [] [] [] delai in ()
                                         
(*mesure de la distance totale*)
let totdist=ref 0.

(*deplacement des drones*)          
let move points speeds obstacle fichmem fin=
  for i=0 to dim-1 do
    if active.(i) then begin
      Printf.fprintf fichmem "%f %f\n" points.(i).x points.(i).y;
      totdist:= !totdist+. sqrt (scal speeds.(i) speeds.(i));
      let newx=points.(i).x +. pas*.speeds.(i).x
      and newy=points.(i).y +. pas*.speeds.(i).y in
      if not (Array.fold_left (fun tf o -> tf || is_inside {x=newx;y=newy} o) false obstacle) then 
        begin
          points.(i).x <- newx;
          points.(i).y <- newy;
        end
      else Printf.printf "ENTREE DANS OBSTACLE\n";
      if dist2 points.(i) dests.(i) < 4.*.norme2 then (active.(i) <- false;incr fin);
      Printf.fprintf fichmem "%f %f\n\n" points.(i).x points.(i).y; flush fichmem
    end
  done

let mini=ref (1./.0.)

(* Vecteur d'échappement *)       
let vectu pa sa pb sb tau=
  debug:=false;
  let pr={x=pb.x-.pa.x;y=pb.y-.pa.y} in
  let (npr,npa)=normangle(pr) in
  mini:= min !mini npr;
  let prbis={x=pr.x/.tau;y=pr.y/.tau} in
  let nprbis=sqrt (scal prbis prbis) in
  let vr={x=sa.x-.sb.x;y=sa.y-.sb.y} in
  let (nvr,nva)=normangle(vr) in  
  
  let vrbis={x=vr.x-.prbis.x;y=vr.y-.prbis.y} in
  let nvrbis=sqrt (scal vrbis vrbis) in
  
  let region_neutre vrbis nvrbis =
    let rayon=(2.*.norme/.tau)-.nvrbis in
    let res={x= rayon*.vrbis.x/.nvrbis/.2.;y=rayon*.vrbis.y/.nvrbis/.2.} in
(*    Printf.printf "NEUTRE";*)
    if rayon>=0. then Some (res,res) else 
      Some (res,opp res)  in 
(*  (res,opp res)  in *)

  if npr<2.*.norme then   (* conflit entre avions *)
    begin
      Printf.printf "%f %f %f %f\n" pa.x pa.y pb.x pb.y;flush stdout;
(*      while true do () done; *)
      failwith "npr<2*norme";
(*      region_neutre vrbis nvrbis  *)
    end
  else 
    begin
      let teta=  asin (2.*.norme/.npr) in
      let npa1=npa-.teta
      and npa2=npa+.teta in
      let p1={x=nprbis*. cos npa1;y=nprbis*. sin npa1}
      and p2={x=nprbis*. cos npa2;y=nprbis*. sin npa2} in
      
      if (pvect p1 prbis vr>0.) && (pvect vr prbis pr>=0.) then 
	begin
	  if !debug then (Printf.printf "SECTEUR 1\n";flush stdout);
	  let alpha=nva-.npa1 in
	  let nvrsa=(nvr *. sin alpha) in
	  let abs_nvrsa=abs_float nvrsa in
	  let u= {x=(cos (npa1-. pi/.2.))/.2.;
		  y=(sin (npa1-. pi/.2.))/.2.} in
	  Some ({x=nvrsa*.u.x;y=nvrsa*.u.y},{x=abs_nvrsa*.u.x;y=abs_nvrsa*.u.y});
	end
      else if (pvect pr prbis vr >0.) && (pvect vr prbis p2 >=0.) then 
	begin
	  if !debug then (Printf.printf "SECTEUR 2\n"  ;flush stdout);
	  let alpha=npa2-.nva in
	  let nvrsa=(nvr *. sin alpha) in
	  let abs_nvrsa=abs_float nvrsa in
	  let u= {x=(cos (npa2+. pi/.2.))/.2.;
		  y=(sin (npa2+. pi/.2.))/.2.} in
	  Some ({x=nvrsa*.u.x;y=nvrsa*.u.y},{x=abs_nvrsa*.u.x;y=abs_nvrsa*.u.y});
	end
      else
	region_neutre vrbis nvrbis 
    end

(*boite des vitesses possibles version circulaire à facettes*)
let speedbox _point speed _speedopt sizelong _sizelat _ns=
  let _d,angle=normangle speed in
  (*  let nspeed={x=ns*.speed.x/.d;y=ns*.speed.y/.d} in *)
  let nbf=float nb in
  let box=Array.init (nb+1) (fun i -> let na=angle+.(float i)*.2.*.pi/.nbf in
                                      {x=sizelong*.cos na;y=sizelong*.sin na}) in
  (*  if is_inside {x=speed.x;y=speed.y} (Array.to_list box) then Printf.printf "ISINSIDE\n";flush stdout; *)
  Array.to_list box

(*boite des vitesses possibles version rectangulaire*) 
(* let speedbox0 _point speed _speedopt sizelong sizelat ns =
  let d=norm speed in
  let nspeed={x=ns*.speed.x/.d;y=ns*.speed.y/.d} in
  let nv={x=sizelong*.nspeed.x;y=sizelong*.nspeed.y} in
  let nu={x= sizelat*.(0.-.nspeed.y);y=sizelat*.nspeed.x} in
  let box=
    [{x=nspeed.x-.(nv.x+.nu.x);y=nspeed.y-.(nv.y+.nu.y)};
     {x=nspeed.x+.(nv.x-.nu.x);y=nspeed.y+.(nv.y-.nu.y)};
     {x=nspeed.x+.(nv.x+.nu.x);y=nspeed.y+.(nv.y+.nu.y)};
     {x=nspeed.x-.(nv.x-.nu.x);y=nspeed.y-.(nv.y-.nu.y)};
     {x=nspeed.x-.(nv.x+.nu.x);y=nspeed.y-.(nv.y+.nu.y)}] in
  box *)
  
exception Fin
exception Echec

(*intersection boite des vitesses et contraintes*)        
let cut i initbox speed constraints = 
  let box=ref initbox in
  if !debug then 
    begin
      List.iter (fun (nv,_qv,_tf) -> 
	Printf.printf "%d %f %f\n" i nv.x nv.y) constraints;
      Printf.printf "\n";flush stdout
    end;
  let delta=ref (-0.5) in
  (try
     while true do
       box:=initbox;
       (try
	  List.iter (fun (nv,qv,tf) ->
	    let r=sqrt (scal nv nv) in
	    let uv={x=qv.x/.r;y=qv.y/.r} in
	    let p={x=speed.x+.nv.x-. if tf then !delta*.uv.x else 0.;
		   y=speed.y+.nv.y-. if tf then !delta*.uv.y else 0.} in
	    box := intersect p qv !box;
	  ) constraints;
	  
	  raise Fin
	with 
	  Vide -> 
	    delta:= !delta+.0.1;
            (*            let _ = Unix.select [] [] [] delai in (); *)
            Printf.printf "delta(%d)= %f\n" i !delta;flush stdout;
	    if !delta>20000. then
	      begin
		Printf.printf "delta(%d)= %f " i !delta;
		Printf.printf "speed=(%f,%f) distmin=%f\n" speed.x speed.y !mini;
		flush stdout;
	       	raise Echec
	      end
	       );
     done;
   with Fin -> ());
  !box

(* projection d'une vitesse sur une boite*)  
let project i speed targetbox =
  if !debug then (Printf.printf "PROJECT\n";flush stdout);
  if is_inside speed targetbox then 
    (if !debug then (Printf.printf "is_inside %d %f %f\n" i speed.x speed.y;flush stdout); 
     speed) 
  else 
    let speedproj=projection speed targetbox in
    speedproj

(* exception Normal of int * int * float * float *)

