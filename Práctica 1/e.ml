
let rec factorial n =
  if n = 0 
    then 1
  else 
    n * factorial (n - 1)

let approximate_e terms =
  let rec sum_series n acc =
    if n > terms 
      then acc
    else
      let term = 1.0 /. float_of_int (factorial n) in
      sum_series (n + 1) (acc +. term)
  in
  sum_series 0 0.0

let () =
  let result = approximate_e 10 in
  Printf.printf "%.10f\n" result