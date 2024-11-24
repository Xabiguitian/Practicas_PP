let is_stable (l, c, r) =
  let rec sorted = function
    | [] -> true
    | [_] -> true
    | x::y::t -> x < y && sorted (y::t)
  in
  let combined = l @ c @ r in
  let expected = List.init (List.length combined) (fun i -> i + 1) in
  sorted l && sorted c && sorted r && List.sort compare combined = expected

let rec all_states n =
  if n = 0 then [([], [], [])]
  else
    let smaller = all_states (n - 1) in
    List.concat [
      List.map (fun (l, c, r) -> (n::l, c, r)) smaller;
      List.map (fun (l, c, r) -> (l, n::c, r)) smaller;
      List.map (fun (l, c, r) -> (l, c, n::r)) smaller
    ]

type move = LtoC | LtoR | CtoL | CtoR | RtoL | RtoC

let move (l, c, r) m =
  let stable (x, y) = match x, y with
    | h::_, h'::_ -> h < h'
    | _, [] -> true
    | _, _ -> false
  in
  match m with
  | LtoC -> if stable (l, c) then (List.tl l, List.hd l :: c, r) else raise (Invalid_argument "move")
  | LtoR -> if stable (l, r) then (List.tl l, c, List.hd l :: r) else raise (Invalid_argument "move")
  | CtoL -> if stable (c, l) then (List.hd c :: l, List.tl c, r) else raise (Invalid_argument "move")
  | CtoR -> if stable (c, r) then (l, List.tl c, List.hd c :: r) else raise (Invalid_argument "move")
  | RtoL -> if stable (r, l) then (List.hd r :: l, c, List.tl r) else raise (Invalid_argument "move")
  | RtoC -> if stable (r, c) then (l, List.hd r :: c, List.tl r) else raise (Invalid_argument "move")

let move_sequence ini moves =
  List.fold_left move ini moves