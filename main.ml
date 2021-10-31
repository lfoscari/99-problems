(* Working with lists *)

(* 1 *)
let rec last = function
    | x :: [] -> Some x
    | _ :: xs -> last xs
    | [] -> None

