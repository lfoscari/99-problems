(* Working with lists *)

(* 1 *)
let rec last = function
    | x :: [] -> Some x
    | _ :: xs -> last xs
    | [] -> None

(* 2 *)
let rec last_two = function
    | x :: y :: [] -> Some (x, y)
    | _ :: xs -> last_two xs
    | [] -> None

(* 3 *)
let rec at index ls =
    match ls with
    | x :: _  when index == 1 -> Some x
    | _ :: xs when index > 1 -> at ( index - 1 ) xs
    | _ -> None
