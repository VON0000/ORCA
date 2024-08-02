open Geom

let obstacle =
  [|
    List.rev
      [
        Geom.create_t 50.000000 (-100.000000);
        Geom.create_t 50.000000 200.000000;
        Geom.create_t 100.000000 200.000000;
        Geom.create_t 100.000000 (-100.000000);
        Geom.create_t 50.000000 (-100.000000);
      ];
    List.rev
      [
        Geom.create_t (-150.000000) (-120.000000);
        Geom.create_t (-150.000000) 10.000000;
        Geom.create_t 10.000000 10.000000;
        Geom.create_t 10.000000 (-120.000000);
        Geom.create_t (-150.000000) (-120.000000);
      ];
    List.rev
      [
        Geom.create_t (-100.000000) 50.000000;
        Geom.create_t (-100.000000) 100.000000;
        Geom.create_t (-50.000000) 100.000000;
        Geom.create_t (-50.000000) 50.000000;
        Geom.create_t (-100.000000) 50.000000;
      ];
  |]

let is_inside a l =
  match l with
  | [] -> false
  | hd :: _ ->
      let rec is_in a l =
        match l with
        | b :: (c :: _ as ctl) -> Geom.pvect_2d b c a < 0. && is_in a ctl
        | [ b ] -> Geom.pvect_2d b hd a <= 0.
        | _ -> failwith "is_inside: unreachable"
      in
      is_in a l
