let rec mcd_pasos x y =
  let rec aux x y pasos =
    if y = 0 then (x, pasos)
    else aux y (x mod y) (pasos + 1)
  in aux x y 1;;