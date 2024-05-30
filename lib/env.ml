(* lib/env.ml *)
let obstacle =
  [|
    List.rev
      [
        Vector.create_vect 50.000000 (-100.000000) 0.;
        Vector.create_vect 50.000000 200.000000 0.;
        Vector.create_vect 100.000000 200.000000 0.;
        Vector.create_vect 100.000000 (-100.000000) 0.;
        Vector.create_vect 50.000000 (-100.000000) 0.;
      ];
    List.rev
      [
        Vector.create_vect (-150.000000) (-120.000000) 0.;
        Vector.create_vect (-150.000000) 10.000000 0.;
        Vector.create_vect 10.000000 10.000000 0.;
        Vector.create_vect 10.000000 (-120.000000) 0.;
        Vector.create_vect (-150.000000) (-120.000000) 0.;
      ];
    List.rev
      [
        Vector.create_vect (-100.000000) 50.000000 0.;
        Vector.create_vect (-100.000000) 100.000000 0.;
        Vector.create_vect (-50.000000) 100.000000 0.;
        Vector.create_vect (-50.000000) 50.000000 0.;
        Vector.create_vect (-100.000000) 50.000000 0.;
      ];
  |]
