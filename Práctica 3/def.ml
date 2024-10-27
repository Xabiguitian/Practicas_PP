
let pi = 4.0 *. atan 1.0

let e = exp 1.0

let max_int_f = float_of_int max_int

let per = function
  | r when r >= 0.0 -> 2.0 *. pi *. r
  | _ -> 0.0 

let area = function
  | r when r >= 0.0 -> pi *. r *. r
  | _ -> 0.0 

let next_char = function
  | c -> char_of_int ((int_of_char c) + 1)

let absf = function
  | x when x >= 0.0 -> x
  | x -> -.x

let odd = function
  | n -> n mod 2 <> 0

let next5mult = function
  | n -> ((n + 4) / 5) * 5

let is_letter = function
  | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') -> true
  | _ -> false

let string_of_bool = function
  | true -> "verdadero"
  | false -> "falso"