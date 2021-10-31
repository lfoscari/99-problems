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
    | x :: _  when index = 1 -> Some x
    | _ :: xs when index > 1 -> at ( index - 1 ) xs
    | _ -> None

(* 4 *)
let length ls =
    let rec aux n = function
        | _ :: xs -> aux ( n + 1 ) xs
        | [] -> n
    in aux 0 ls

(* 5 *)
let rev ls =
    let rec aux acc = function
        | x :: xs -> aux ( x :: acc ) xs
        | [] -> acc
    in aux [] ls

(* 6 *)
let is_palindrome ls =
    rev ls = ls

(* 7 *)
type 'a node =
    | One of 'a
    | Many of 'a node list

let flatten s =
    let rec aux acc = function
        | ( One x ) :: xs -> aux ( x :: acc ) xs
        | ( Many x ) :: xs -> aux ( ( aux [] x ) @ acc ) xs
        | [] -> acc
    in aux [] s |> rev

(* 8 *)
let rec compress = function
    | x :: ( y :: _ as xs ) when x = y -> compress xs
    | x :: xs -> x :: compress xs
    | [] -> []

(* 9 *)
let pack ls =
    let rec aux cur acc = function
        | x :: ( y :: _ as xs ) when x = y -> aux ( x :: cur ) acc xs
        | x :: xs -> aux [] ( ( x :: cur ) :: acc ) xs
        | [] -> acc |> rev
    in aux [] [] ls

(* 10 *)
let encode ls =
    let rec aux acc = function
        | [] -> acc
        | ( x :: xs ) :: rest -> aux ( ( 1 + length xs, x ) :: acc )  rest
        | [] :: _ -> raise Not_found
    in pack ls |> aux [] |> rev

(* 11 *)
type 'a rle =
    | One of 'a
    | Many of int * 'a

let encode ls =
    let rec aux acc = function
        | [] -> acc
        | ( x :: xs ) :: rest -> (
                match ( 1 + length xs ) with
                | 1 -> aux ( One x :: acc ) rest
                | n -> aux ( Many ( n, x ) :: acc ) rest )
        | [] :: _ -> raise Not_found
    in pack ls |> aux [] |> rev

(* 12 *)
let rec decode = function
    | One x :: xs | Many ( 1, x ) :: xs -> x :: decode xs
    | Many ( n, x ) :: xs -> x :: decode ( Many ( n - 1, x ) :: xs )
    | [] -> []

(* 13 *)
let encode ls =
    let rle x = function
        | 0 -> One x
        | n -> Many ( n, x )
    in let rec aux count acc = function
        | x :: ( y :: _ as xs ) when x = y -> aux ( count + 1 ) acc xs
        | x :: xs -> aux 0 ( ( rle x ( count + 1 ) ) :: acc ) xs
        | [] -> acc |> rev
    in aux 0 [] ls

(* 14 *)
let rec duplicate = function
    | x :: xs -> x :: x :: duplicate xs
    | [] -> []

(* 15 *)
let replicate ls count =
    let rec expand x = function
        | 0 -> []
        | n -> x :: expand x ( n - 1 )
    in let rec aux = function
        | x :: xs -> ( expand x count ) @ aux xs
        | [] -> []
    in aux ls

(* 16 *)
let drop ls index =
    let rec aux inc = function
        | _ :: xs when inc = index -> aux 1 xs
        | x :: xs -> x :: aux ( inc + 1 ) xs
        | [] -> []
    in aux 1 ls
