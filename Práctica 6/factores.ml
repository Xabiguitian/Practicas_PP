let factoriza n =
  let rec factoresPrimos num divisor acc =
    if num < 2 then acc
    else if num mod divisor = 0 then
      factoresPrimos (num / divisor) divisor (acc @ [divisor])
    else
      factoresPrimos num (divisor + 1) acc
  in

  if n = 0 then "0"
  else if n = 1 then "1"
  else
    let factores =
      if n < 0 then
        let neg_factors = factoresPrimos (-n) 2 [] in
        (-1) :: neg_factors
      else factoresPrimos n 2 []
    in
    String.concat " * " (List.map string_of_int factores)