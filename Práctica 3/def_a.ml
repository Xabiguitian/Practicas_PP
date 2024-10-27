let pi = 4.0 *. atan 1.0;;

let e = exp 1.0;;

let max_int_f = float_of_int max_int;;

let per r = 2. *. pi *.r;;

let area r = pi *. r *. r;;

let next_char c = char_of_int ((int_of_char c) + 1);;

let absf x = if x>0. then x else (-.x);;

let odd n = n mod 2 <> 0;;

let next5mult n = ((n + 4) / 5) * 5;;

let is_letter c = if (c>= 'a' && c<='z') || (c >= 'A' && c <= 'Z') then true else false;;

let string_of_bool b = if b then "verdadero" else "falso"