let rec sumto n = if n <= 0 then 0 else n + sumto (n - 1)


let rec exp2 n = if n = 0 then 1 else 2 * exp2 (n - 1)


let num_cifras n =
  let n = abs n in 
  let rec aux n count = if n = 0 then count else aux (n / 10) (count + 1) in if n = 0 then 1 else aux n 0


let sum_cifras n =
  let n = abs n in 
  let rec aux n sum = if n = 0 then sum else aux (n / 10) (sum + (n mod 10)) in aux n 0