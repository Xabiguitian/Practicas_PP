(* Define a función binstr_of_int para converter un enteiro positivo á súa representación binaria en string *)
let rec binstr_of_int n =
  if n = 0 then "0"
  else if n = 1 then "1"
  else (binstr_of_int (n / 2)) ^ (string_of_int (n mod 2))

(* Define a función int_of_binstr para converter unha cadea binaria a un enteiro, devolvendo o módulo de Sys.int_size *)
let int_of_binstr s =
  let len = String.length s in
  let rec aux i acc =
    if i = len then acc
    else
      let bit = if s.[i] = '1' then 1 else 0 in
      aux (i + 1) ((2 * acc + bit) mod (1 lsl Sys.int_size))
  in aux 0 0

(* Define a función int_of_binstr' que ignora os espazos en branco na cadea binaria *)
let int_of_binstr' s =
  let len = String.length s in
  let rec aux i acc =
    if i = len then acc
    else
      let acc' =
        if s.[i] = ' ' then acc
        else
          let bit = if s.[i] = '1' then 1 else 0 in
          (2 * acc + bit) mod (1 lsl Sys.int_size)
      in aux (i + 1) acc'
  in aux 0 0
