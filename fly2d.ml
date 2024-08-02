(* lib/fly2d.ml *)
open Aircraft

let is_stop dim fin = 
  if fin < dim then true else false

let run =
  let fin = ref 0 in
  let acfts = Aircraft.get_acft_lst Const.dim in
  (
      while is_stop Const.dim fin do
          move
      done
)
