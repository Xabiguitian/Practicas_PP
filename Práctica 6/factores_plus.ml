let factoriza n =
  let rec prime_factors num divisor acc =
    if num < 2 then acc
    else if num mod divisor = 0 then
      match acc with
      | (d, count) :: rest when d = divisor -> prime_factors (num / divisor) divisor ((d, count + 1) :: rest)
      | _ -> prime_factors (num / divisor) divisor ((divisor, 1) :: acc)
    else
      prime_factors num (divisor + 1) acc
  in

  if n = 0 then "0"
  else if n = 1 then "1"
  else
    let factors =
      if n < 0 then
        (-1, 1) :: (List.rev (prime_factors (-n) 2 []))
      else
        List.rev (prime_factors n 2 [])
    in

    let factor_to_string (factor, count) =
      if count = 1 then string_of_int factor
      else Printf.sprintf "%d^%d" factor count
    in
    String.concat " * " (List.map factor_to_string factors)