let i_prod l = List.fold_left  (fun acc x -> x * acc) 1 l;;

let f_prod l = List.fold_left  (fun acc x -> x *. acc) 1. l;;

let lmin = function
  | [] -> raise (Failure "lmin")
  | h::t -> List.fold_left min h t

let min_max = function
  | [] -> raise (Failure "min_max")
  | h::t ->
    List.fold_left (fun (mi, ma) x -> (min mi x, max ma x)) (h, h) t

let rev l =
  List.fold_left (fun acc x -> x :: acc) [] l

let rev_append l1 l2 =
  List.fold_left (fun acc x -> x :: acc) l2 l1

let rev_map f l =
  List.fold_left (fun acc x -> (f x) :: acc) [] l

let concat sl =
  List.fold_left (^) "" sl