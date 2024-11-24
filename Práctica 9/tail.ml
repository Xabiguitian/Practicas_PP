let concat l =
  List.fold_left (@) [] l

let rec front = function
  | [] -> raise (Failure "front")
  | [_] -> []
  | h::t -> h :: front t

let rec compress = function
  | h1::h2::t -> if h1 = h2 then compress (h2::t) else h1 :: compress (h2::t)
  | l -> l

let rec ofo = function
  | [] -> []
  | h::t -> h :: List.filter ((<>) h) (ofo t)

let fold_right' f l acc =
  List.fold_left (fun g x -> fun a -> g (f x a)) (fun x -> x) l acc

let rec sublists = function
  | [] -> [[]]
  | h::t -> let st = sublists t in st @ List.map (fun l -> h :: l) st