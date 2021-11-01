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
        | [] -> rev acc
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

(* 17 *)
let split ls size =
    let rec aux acc index = function
        | x :: xs when index < size -> aux ( x :: acc ) ( index + 1 ) xs
        | xs -> rev acc, xs
    in aux [] 0 ls

(* 18 *)
let slice ls l r =
    let rec aux acc index = function
        | _ :: xs when index < l -> aux acc ( index + 1 ) xs
        | x :: xs when index <= r -> aux ( x :: acc ) ( index + 1 ) xs
        | _ -> rev acc
    in aux [] 0 ls

(* 19 *)
let rec rotate ls n =
    match n, ls with
    | 0, _ | _, [] -> ls
    | _n, _ when _n < 0 -> rotate ls ( ( length ls ) + _n )
    | _n, x :: xs -> rotate ( xs @ [x] ) ( _n - 1 )

(* 20 *)
let rec remove_at index = function
    | x :: xs when index = 0 -> xs
    | x :: xs -> x :: ( remove_at ( index - 1 ) xs )
    | [] -> []

(* 21 *)
let rec insert_at y index = function
    | xs when index = 0 -> y :: xs
    | x :: xs -> x :: ( insert_at y ( index - 1 ) xs )
    | [] -> []

(* 22 *)
let rec range current = function
    | final when current = final -> [current]
    | final -> current :: range ( current + 1 ) final

(* 23 *)
let rec rand_select ls = function
    | 0 -> []
    | amount ->
        let r = length ls |> Random.int in (
        match at ( r + 1 ) ls with
        | Some x -> x :: ( rand_select ( remove_at r ls ) ( amount - 1 ) )
        | None -> raise Not_found )

(* 24 *)
let rec lotto_select amount limit =
    rand_select ( range 1 limit ) amount

(* 25 *)
let permutation ls =
    rand_select ls ( length ls )

(* 26 *)
let rec add x = List.map ( fun xs -> x :: xs )

let rec extract size = function
    | xs when size = 1 -> List.map ( fun x -> [x] ) xs
    | x :: xs -> ( extract ( size - 1 ) xs |> add x ) @ ( extract size xs )
    | [] -> []

(* 27 *)
let rec group ls = function
    | [] -> raise Not_found
    | g :: [] -> extract g ls |> List.map ( fun xs -> [xs] )
    | g :: gs ->
        List.map ( fun xs ->
            let neg = List.filter ( fun x -> not ( List.mem x xs ) ) ls in (* ["c"; "d"] *)
            add xs ( group neg gs )
        ) ( extract g ls ) |> List.flatten

(* 28 *)
let length_sort =
    List.sort ( fun a b -> compare ( length a ) ( length b ) )

(* 29 *)
let frequency_sort ls =
    let count x ls = List.filter ( fun y -> x = y ) ls |> length in
    let lengths = List.map length ls in
    List.sort ( fun a b ->
        compare ( count ( length a ) lengths ) ( count ( length b ) lengths ) ) ls


(* Arithmetic *)

(* 31 *)
let is_prime x =
    let rec aux = function
        | y when y = x -> true
        | y when y > x || x mod y = 0 -> false
        | y -> aux ( y + 1 )
    in aux 2

(* 32 *)
let rec gcd x y =
    if x = y then x
    else if x > y then gcd ( x - y ) y
    else gcd ( y - x ) x

(* 33 *)
let coprime x y =
    gcd x y = 1

(* 34 *)
let phi x =
    let rec aux acc cur =
        if cur = x then acc
        else if coprime cur x then aux ( acc + 1 ) ( cur + 1 )
        else aux acc ( cur + 1 )
    in aux 0 1

(* 35 *)
let factors x =
    let rec aux acc x = function
        | y when y > x -> rev acc
        | y when x mod y = 0 -> aux ( y :: acc ) ( x / y ) 2
        | y -> aux acc x ( y + 1 )
    in aux [] x 2
