let from0to n =
  if n < 0 then raise (Failure "from0to")
  else List.init (n + 1) (fun x -> x)

let to0from n =
  if n < 0 then raise (Failure "to0from")
  else List.init (n + 1) (fun x -> n - x)

let pair x l =
  List.map (fun y -> (x, y)) l

let remove_first x l =
  let rec aux acc = function
    | [] -> List.rev acc
    | h::t -> if h = x then List.rev_append acc t else aux (h :: acc) t
  in aux [] l

let remove_all x l =
  List.filter (fun y -> y <> x) l

let ldif l1 l2 =
  List.filter (fun x -> not (List.mem x l2)) l1