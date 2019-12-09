(*za vajo *)

let odgovor = 5

let pi = 4.0 *. atan 1.0;; 

let kvadriraj n = n * n;;

let zmnozi x y = x * y;;

let dvakrat_na_nic f = f (f 0);;

let pozdravi ime = 
    match ime with
    | "Matija" -> "Dober dan, gospod predavatelj!"
    | "Filip" | "Žiga" -> "Oj!"
    | _ -> "Dober dan, " ^ ime ^ "!" 


let rec fakulteta n =
    match n with 
    | 0 -> 1 
    | n -> n * fakulteta(n - 1)

let rec fakulteta1  = function 
    | 0 -> 1 
    | n -> n * fakulteta(n - 1)

let rec fibonacci = function 
    | 0 -> 0 
    | 1 -> 1
    | n -> fibonacci(n-1) + fibonacci(n-2) 


let rec je_sodo = function
  | 0 -> true
  | n -> je_liho (n - 1)

and je_liho = function
  | 0 -> false
  | n -> je_sodo (n - 1)

let a = [1; 2; 3; 4];;

let b = ['a'; 'b'; 'c'; 'd'];;

let c = 1 :: 2 :: [3; 4];; 

let rec dolzina xs = 
    match xs with
    | [] -> 0
    | x :: xs' -> 1 + dolzina xs'

let rec vsota xs = 
    match xs with 
    | [] -> 0 
    | x :: xs' -> x + vsota xs' 

let rec zmnozi xs = 
    match xs with
    | [] -> 1
    | x :: xs' -> x * zmnozi xs' 

(* morš dat 1, ker na koncu bo itak prazen in da ti ga pomnoži z 1*)

 (*# double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]*)



let is_root n k = if n*n == k then true else false 