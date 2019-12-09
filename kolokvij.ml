(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)


let is_root n k =
    if n*n = k then true else false 


let pack3 x y z = (x , y, z)

 (*pack3 1 false [];;*)

let rec sum_if_not f  = function
    | [] -> 0
    | x :: xs -> if f x then sum_if_not f xs else x + (sum_if_not f xs)

(*
let apply list1 list2 = 
    match list1, list2 with 
    | []
*)
(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)


type vrsta_srecanja = 
    | Predavanja  
    | Vaje

type srecanje = {predmet : string; vrsta : string; trajanje : float}

(*ni dela glih
let primer = {predmet = "Analiza 2a", vrsta = "vaje", trajanje = 3}
*)

let vaje () = failwith "dopolni me"

let predavanje () = failwith "dopolni me"

let urnik_profesor () = failwith "dopolni me"

let je_preobremenjen () = failwith "dopolni me"

let bogastvo () = failwith "dopolni me"