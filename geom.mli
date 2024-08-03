type t = { x : float; y : float; }
val create_t : float -> float -> t
<<<<<<< HEAD:geom.mli
val default_t : t
=======
>>>>>>> 485f8f8 (build: ocamlc and ocamlopt):lib/geom.mli
val mean_2d : t -> t -> float * float
val diff_2d : t -> t -> t
val scal_2d : t -> t -> float
val vect_2d : t -> t -> float
val norm_2d : t -> float
val angle_2d : t -> float
<<<<<<< HEAD:geom.mli
val heading_angle : t -> t -> float
=======
>>>>>>> 485f8f8 (build: ocamlc and ocamlopt):lib/geom.mli
val dist2_2d : t -> t -> float
val find_cap_2d : t -> t -> float
val opp_2d : t -> t
val pvect_2d : t -> t -> t -> float
val determ_2d : t -> t -> float
