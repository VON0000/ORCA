module type AircraftSig = sig
  type t
  val create : int -> t list -> t
  val get_position : t -> Geom.t
  val get_dest : t -> Geom.t
  val get_speed : t -> Geom.t
  val get_speedopt : t -> Geom.t
  val get_route : t -> Geom.t list
  val is_active : t -> bool
end

