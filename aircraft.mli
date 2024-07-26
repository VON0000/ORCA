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