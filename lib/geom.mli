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
