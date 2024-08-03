<<<<<<< HEAD:aircraft.mli
type t = {
  position : Geom.t;
  dest : Geom.t;
  speed : Geom.t;
  speedopt : Geom.t;
  route : Geom.t list;
  active : bool;
}

val get_position : t -> Geom.t
val get_dest : t -> Geom.t
val get_speed : t -> Geom.t
val get_speedopt : t -> Geom.t
val get_route : t -> Geom.t list
val get_active : t -> bool
val get_arft_lst : int -> t list ref
=======
type t = { x : float; y : float; }
val create_t : float -> float -> t
val mean_2d : t -> t -> float * float
val diff_2d : t -> t -> t
val scal_2d : t -> t -> float
val vect_2d : t -> t -> float
val norm_2d : t -> float
val angle_2d : t -> float
val dist2_2d : t -> t -> float
val find_cap_2d : t -> t -> float
val opp_2d : t -> t
val pvect_2d : t -> t -> t -> float
val determ_2d : t -> t -> float
>>>>>>> 485f8f8 (build: ocamlc and ocamlopt):lib/aircraft.mli
