type vect = { mutable x : float; mutable y : float; }
type acft = { mutable p : vect; mutable s : vect; d : vect; }
val create_vect : float -> float -> vect
val create_acft : vect -> vect -> vect -> acft
val pi : float
val debug : bool ref
val mean : vect -> vect -> vect
val diff : vect -> vect -> vect
val scal : vect -> vect -> float
val vect : vect -> vect -> float
val normangle : vect -> float * float
val find_cap : vect -> vect -> float
val opp : vect -> vect
val norm : vect -> float
val determ : float -> float -> float -> float -> float
val epsilon : float
exception Droites_confondues
exception No_solution
val normalize : vect -> vect
val solve : vect -> vect -> vect -> vect -> vect
val inside : vect -> vect -> vect -> bool
val complete : 'a list -> 'a list
exception Vide
val intersect : vect -> vect -> vect list -> vect list
val pvect : vect -> vect -> vect -> float
val pscal : vect -> vect -> vect -> float
val is_inside : vect -> vect list -> bool
val equal_points : vect -> vect -> bool
val dist2 : vect -> vect -> float
val dist : vect -> vect -> float
val proj_pt_seg : vect -> vect -> vect -> float * vect
val proj_pt_vec : vect -> vect -> vect -> vect
val cross_segs : vect -> vect -> vect -> vect -> bool
val cross_segconv : vect -> vect -> vect list -> bool
val projection : vect -> vect list -> vect
val projmax : vect -> vect list -> vect
val rotate : float -> vect -> float -> vect
val extremes : vect -> vect list -> vect * vect * vect * vect
val box : vect list
val box2 : vect list
val outc : out_channel
val test :
  unit -> Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
