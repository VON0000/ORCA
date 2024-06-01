let dim = Const.dim
let arfts = Aircraft.get_arft dim
let fin = ref 0

let fly2d () =
  while !fin < dim do
    for i = 0 to dim - 1 do
      if arfts.(i).active then ()
    done
  done
