let otro ori des =
  6 - ori - des

let move (ori, des) =
  match (ori, des) with
  | (1, 2) -> " 1 -> 2    3 \n"
  | (1, 3) -> " 1 ---2--> 3 \n"
  | (2, 1) -> " 1 <- 2    3 \n"
  | (2, 3) -> " 1    2 -> 3 \n"
  | (3, 1) -> " 1 <--2--- 3 \n"
  | (3, 2) -> " 1    2 <- 3 \n"
  | _ -> failwith "Invalid inputs"

let rec hanoi n ori des =
  (* n número de discos, 1 <= ori <= 3, 1 <= dest <= 3, ori <> des *)
  if n = 0 then "" else
  let otro = otro ori des in
  hanoi (n-1) ori otro ^ move (ori, des) ^ hanoi (n-1) otro des

let hanoi n ori des =
  if n = 0 || ori = des then "\n"
  else hanoi n ori des

let print_hanoi n ori des =
  if n < 0 || ori < 1 || ori > 3 || des < 1 || des > 3
  then print_endline " ** ERROR ** \n"
  else print_endline (" =========== \n" ^
  hanoi n ori des ^
  " =========== \n");;

let crono f x = 
    let t = Sys.time () in
    f x; Sys.time () -. t

let rec nth_hanoi_move n nd ori des =
    if nd = 1 then
      if n = 1 then (ori, des) else failwith "Invalid move number"
    else
      let total_moves = (1 lsl (nd - 1)) - 1 in
      let otro_p = otro ori des in
      if n <= total_moves then
        nth_hanoi_move n (nd - 1) ori otro_p
      else if n = total_moves + 1 then
        (ori, des)
      else
        nth_hanoi_move (n - total_moves - 1) (nd - 1) otro_p des

let hanoi_test () =
    for i = 20 to 25 do
        let tiempo = crono (fun () -> hanoi i 1 3) () in
        Printf.printf "Tiempo para hanoi con %d discos: %f segundos\n" i tiempo
    done

let nth_hanoi_move_test () =
    let nd = 63 in
    let tiempos = List.map (fun n -> (n, crono (fun () -> nth_hanoi_move n nd 1 3) ())) [1; 1000; 1000000] in
    List.iter (fun (n, t) -> Printf.printf "Tiempo para nth_hanoi_move con nd=%d y n=%d: %f segundos\n" nd n t) tiempos

let () =
    Printf.printf "Pruebas para hanoi:\n";
    hanoi_test ();
    Printf.printf "\nPruebas para nth_hanoi_move:\n";
    nth_hanoi_move_test ()
