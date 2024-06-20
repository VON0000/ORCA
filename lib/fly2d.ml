(* lib/fly2d.ml *)
open Aircraft

let run =
  let arfts = Aircraft.get_arft_lst Const.dim in
  Aircraft.read_acft arfts
