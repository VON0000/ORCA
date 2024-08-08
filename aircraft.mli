type t = {
  mutable position : Geom.t;
  dest : Geom.t;
  speed : Geom.t;
  mutable speedopt : Geom.t;
  route : Geom.t list;
  mutable active : bool;
}

val update_speedopt : t -> Geom.t
val get_position : t -> Geom.t
val get_dest : t -> Geom.t
val get_speed : t -> Geom.t
val get_speedopt : t -> Geom.t
val get_route : t -> Geom.t list
val get_active : t -> bool
val acft_lst : t list ref
val move_one : t -> unit
