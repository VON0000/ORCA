type acft = {
  mutable p : Vector.vect;
  mutable s : Vector.vect;
  d : Vector.vect;
}

val create_acft : Vector.vect -> Vector.vect -> Vector.vect -> acft