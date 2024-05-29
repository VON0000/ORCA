(* lib/aircraft.ml *)
(* p=position; s=speed; d=dest *)
type acft = {
  mutable p : Vector.vect;
  mutable s : Vector.vect;
  d : Vector.vect;
}

let create_acft p s d = { p; s; d }