let dist2 (a : Vector.vect) (b : Vector.vect) =
  let dx = a.x -. b.x and dy = a.y -. b.y in
  (dx *. dx) +. (dy *. dy)