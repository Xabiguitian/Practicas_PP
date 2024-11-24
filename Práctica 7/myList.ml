let hd = function
  | [] -> raise (Failure "hd")
  | h::_ -> h

let tl = function
  | [] -> raise (Failure "tl")
  | _::t -> t
let rec last = function
  | [] -> raise (Failure "last")
  | [x] -> x
  | _::t -> last t

let rec length = function
  | [] -> 0
  | _::t -> 1 + length t

let length' l =
  let rec aux acc = function
    | [] -> acc
    | _::t -> aux (acc + 1) t
  in aux 0 l

let rec compare_lengths l1 l2 =
  match (l1, l2) with
  | ([], []) -> 0
  | (_::_, []) -> 1
  | ([], _::_) -> -1
  | (_::t1, _::t2) -> compare_lengths t1 t2

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> h :: append t l2

let rev_append l1 l2 =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h :: acc) t
  in aux l2 l1

let rev l =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h :: acc) t
  in aux [] l

let rec concat = function
  | [] -> []
  | h::t -> append h (concat t)

let flatten = concat

let init n f =
  let rec aux i acc =
    if i >= n then rev acc
    else aux (i + 1) (f i :: acc)
  in aux 0 []

let rec nth l n =
  match (l, n) with
  | ([], _) -> raise (Failure "nth")
  | (_, n) when n < 0 -> raise (Invalid_argument "nth")
  | (h::_, 0) -> h
  | (_::t, n) -> nth t (n - 1)

let rec map f = function
  | [] -> []
  | h::t -> f h :: map f t

let rev_map f l =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (f h :: acc) t
  in aux [] l

let rec map2 f l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | ([], _) | (_, []) -> raise (Invalid_argument "map2")
  | (h1::t1, h2::t2) -> f h1 h2 :: map2 f t1 t2

let rec combine l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | ([], _) | (_, []) -> raise (Invalid_argument "combine")
  | (h1::t1, h2::t2) -> (h1, h2) :: combine t1 t2

let rec split = function
  | [] -> ([], [])
  | (x, y)::t ->
      let (xs, ys) = split t in
      (x::xs, y::ys)

let rec find f = function
  | [] -> raise Not_found
  | h::t -> if f h then h else find f t

let rec filter f = function
  | [] -> []
  | h::t -> if f h then h :: filter f t else filter f t

let filter' f l =
  let rec aux acc = function
    | [] -> List.rev acc
    | h::t -> aux (if f h then h::acc else acc) t
  in aux [] l

let rec partition f = function
  | [] -> ([], [])
  | h::t ->
      let (yes, no) = partition f t in
      if f h then (h::yes, no) else (yes, h::no)

let partition' f l =
  let rec aux acc1 acc2 = function
    | [] -> (List.rev acc1, List.rev acc2)
    | h::t -> if f h then aux (h::acc1) acc2 t else aux acc1 (h::acc2) t
  in aux [] [] l

let rec for_all f = function
  | [] -> true
  | h::t -> f h && for_all f t

let rec exists f = function
  | [] -> false
  | h::t -> f h || exists f t

let rec mem x = function
  | [] -> false
  | h::t -> h = x || mem x t

let rec fold_left f acc = function
  | [] -> acc
  | h::t -> fold_left f (f acc h) t

let rec fold_right f l acc =
  match l with
  | [] -> acc
  | h::t -> f h (fold_right f t acc)



  