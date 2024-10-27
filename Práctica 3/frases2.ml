let maximo = max_int;;
(*val maximo : int = 4611686018427387903*)

let minimo = min_int;;
(*val minimo : int = -4611686018427387904*)

minimo + maximo;;
(*- : int = -1*)

minimo + maximo + 1;;
(*- : int = 0*)

maximo + 1;;
(*- : int = -4611686018427387904*)

minimo = maximo + 1;;
(*- : bool = true*)

2 * minimo;;
(*- : int = 0*)

minimo - 1 = maximo;;
(*- : bool = true*)

2 * maximo;;
(*- : int = -2*)

let maximo = 1. /. 0.;;
(*val maximo : float = infinity*)

let minimo = -1.0 /. 0.;;
(*val minimo : float = neg_infinity*)

1. /. maximo;;
(*- : float = 0.*)

1. /. minimo;;
(*- : float = -0.*)

1. /. maximo = 1. /. minimo;;
(*- : bool = true*)

0. /. 0.;;
(*- : float = nan*)

maximo +. maximo;;
(*- : float = infinity*)

maximo -. maximo;;
(*- : float = nan*)

-. maximo = minimo;;
(*- : bool = true*)

maximo +. minimo;;
(*- : float = nan*)

not (minimo < maximo);;
(*- : bool = false*)

let not = "no";;
(*val not : string = "no"*)

(*not (minimo < maximo);;*)
(*Error de tipo ya que ahora not es un string no una negación*)

Stdlib.not (minimo < maximo);;
(*- : bool = false*)

not ^ not;;
(*- : string = "nono"*)

(function not ->not^not) ("si");;
(*- : string = "sisi"*)

not;;
(*- : string = "no"*)

let x = 1;;
(*val x : int = 1*)

(function x -> x + x * x) (2);;
(*- : int = 6*)

1 + (function x -> x + x * x) (2);;
(*- : int = 7*)

x + (function x -> x * x) (2);;
(*- : int = 5*)

let y = x + (fun x -> x * x) 2;;
(*val y : int = 5*)

(function x -> (function x -> x * x) (3*x)) (x+1);;
(*- : int = 36*)

(function x -> 2 * x);;
(*- : int -> int = <fun>*)

(function x -> 2 * x) (2 + 1);;
(*- : int = 6*)

(function x -> 2 * x) 2 + 1;;
(*- : int = 5*)

(function y -> 2 * y) ((function y -> 2 * y) 3);;
(*- : int = 12*)

let doble = function z -> 2 * z;;
(*val doble : int -> int = <fun>*)

doble 2 + 1;;
(*- : int = 5*)

doble (doble 3);;
(*- : int = 12*)

(*doble doble 3;;*)
(*Error léxico ya que al no tener paréntesis al primer doble se le pasan dos valores y solo requiere de uno*)

abs (1 - 2);;
(*- : int = 1*)

abs 1;;
(*- : int = 1*)

(*abs -1;;*)
(*Error de tipo ya que le faltan los paréntesis*)

let abs = function x -> if x >= 0. then x else -. x;;
(*val abs : float -> float = <fun>*)

(*abs 1;;*)
(*Error de tipo ya que en el comando anterior lo definimos como una función que solicita un número decimal*)

abs 1.5;;
(*- : float = 1.5*)

Stdlib.abs 1;;
(*- : int = 1*)

let suma = function (x,y) -> x + y;;
(*val suma : int * int -> int = <fun>*)

2 * suma (2,3) - suma (1,1);;
(*- : int = 8*)

let suma' = function x -> (function y -> x + y);;
(*val suma' : int -> int -> int = <fun>*)

suma' 3;;
(*- : int -> int = <fun>*)

(suma' 3) 2;;
(*- : int = 5*)

suma' 3 2;;
(*- : int = 5*)

suma (3,2) = suma' 3 2;;
(*- : bool = true*)

(*suma 3;;*)
(*Error sintáctic0 ya que se piden dos valores y solo se pasa uno*)

let suma3 = suma' 3;;
(*val suma3 : int -> int = <fun>*)

suma3 10;;
(*- : int = 13*)

suma3 2 + 1;;
(*- : int = 6*)

suma3 (suma3 10);;
(*- : int = 16*)

(*suma3 suma3 10;;*)
(*Error de tipos causado por no tener los paréntesis*)