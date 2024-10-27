let rec mcd x y =
  if x = 0 then y
  else if y = 0 then x
  else if x > y then mcd (x - y) y
  else mcd x (y - x);;

let rec mcd' x y =
  if y = 0 then x
  else mcd' y (x mod y);;