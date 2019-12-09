(* ========== Vaje 5: Urejanje  ========== *)


(*----------------------------------------------------------------------------*]
 Funkcija [randlist len max] generira seznam dolžine [len] z naključnimi
 celimi števili med 0 in [max].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let l = randlist 10 10 ;;
 val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
[*----------------------------------------------------------------------------*)

let rec randlist len max = 
    let rec listgen len acc = 
         if len <= 0 then acc 
         else listgen(len -1) (Random.int max :: acc)
    in 
    listgen len []

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Sedaj lahko s pomočjo [randlist] primerjamo našo urejevalno funkcijo (imenovana
 [our_sort] v spodnjem primeru) z urejevalno funkcijo modula [List]. Prav tako
 lahko na manjšem seznamu preverimo v čem je problem.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 let test = (randlist 100 100) in (our_sort test = List.sort compare test);;
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*Pove, ali funkcija [our_sort] pravilno uredi naključni seznam dolžine [len]]*)
let rec tester our_sort len = 
    let test = randlist len 10000 in 
    our_sort test = List.sort compare test 

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Vstavljanjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert y xs] vstavi [y] v že urejen seznam [xs] in vrne urejen
 seznam. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 9 [0; 2];;
 - : int list = [0; 2; 9]
 # insert 1 [4; 5];;
 - : int list = [1; 4; 5]
 # insert 7 [];;
 - : int list = [7]
[*----------------------------------------------------------------------------*)

let rec insert y = function
    | [] -> [y]
    | x :: xs -> if x < y then x :: insert y xs else y :: x :: xs 

let rec insert_tlrec y xs = 
    let rec insert'  acc = function
        | [] -> List.rev (y :: acc)
        | x :: xs -> 
            if x < y then
                insert' (x :: acc) xs
            else
                (*list.rev_append xs ys ~ (list.rev xs) @ ys ampak tail recursive. *)
                List.rev_append (y :: acc) (x :: xs)
    in
    insert' [] xs 


(*----------------------------------------------------------------------------*]
 Prazen seznam je že urejen. Funkcija [insert_sort] uredi seznam tako da
 zaporedoma vstavlja vse elemente seznama v prazen seznam.
[*----------------------------------------------------------------------------*)

let rec insert_sort xs = 
    List.fold_left (fun ys y -> insert_tlrec y ys) [] xs 

let rec insert_sort_bad = function 
    | [] -> []
    | x :: xs -> insert x (insert_sort_bad xs)

let rec insert_sort_trlec xs = 
    let rec sort' acc = function
    | [] -> acc 
    | x :: xs -> sort' (insert_tlrec x acc) xs 
    in
    sort' [] xs 
    

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [min_and_rest list] vrne par [Some (z, list')] tako da je [z]
 najmanjši element v [list] in seznam [list'] enak [list] z odstranjeno prvo
 pojavitvijo elementa [z]. V primeru praznega seznama vrne [None]. 
[*----------------------------------------------------------------------------*)
(*
let min_and_rest list = 
    | [] -> None
    | x :: xs -> 
        let rec get_min acc rest = function
        | [] -> (acc, rest)
        | y :: ys ->
            if acc < y then 
                get_min acc (y :: rest) ys 
            else
                get_min y (acc :: rest) ys
        in 
        Some (get_min x [] xs) 
*)
let rec min_and_rest_2 xs = 
    let rec get_min default = function
    | [] -> default
    | x :: xs -> min x (get_min default xs)
    in 
    let rec remove y = function
    | [] -> []
    | x :: xs when x = y -> xs 
    | x :: xs -> x :: remove y xs 
    in 
    match xs with
    | [] -> None
    | x :: xs -> 
        let m = get_min x xs in 
        let rest = remove m (x :: xs) in 
        Some (m,rest)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri urejanju z izbiranjem na vsakem koraku ločimo dva podseznama, kjer je prvi
 že urejen, drugi pa vsebuje vse elemente, ki jih je še potrebno urediti. Nato
 zaporedoma prenašamo najmanjši element neurejenega podseznama v urejen
 podseznam, dokler ne uredimo vseh. 

 Če pričnemo z praznim urejenim podseznamom, vemo, da so na vsakem koraku vsi
 elementi neurejenega podseznama večji ali enaki elementom urejenega podseznama,
 saj vedno prenesemo najmanjšega. Tako vemo, da moramo naslednji najmanjši člen
 dodati na konec urejenega podseznama.
 (Hitreje je obrniti vrstni red seznama kot na vsakem koraku uporabiti [@].)
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

let rec selection_sort_2 xs = 
    match min_and_rest_2 xs with 
    | None -> []
    | Some (m, rest) -> m :: selection_sort_2 rest
    
(*tle bi lah tut brez 2 pač, sam tisto prvo funkcijo neki čudn zazna in pač
sem uporabil drugo funkcijo in dela, on je dal selection_sort samo, 
brez dvojke *)


(*let rec selection_sort_tlrec xs = *)

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort] je implementacija zgoraj opisanega algoritma.
 Namig: Uporabi [min_and_rest] iz prejšnje naloge.
[*----------------------------------------------------------------------------*)



(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem na Tabelah
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri delu z tabelami (array) namesto seznami, lahko urejanje z izbiranjem 
 naredimo "na mestu", t.j. brez uporabe vmesnih kopij (delov) vhoda. Kot prej
 tabelo ločujemo na že urejen del in še neurejen del, le da tokrat vse elemente
 hranimo v vhodni tabeli, mejo med deloma pa hranimo v spremenljivki
 [boundary_sorted]. Na vsakem koraku tako ne izvlečemo najmanjšega elementa
 neurejenga dela tabele temveč poiščemo njegov indeks in ga zamenjamo z
 elementom na meji med deloma (in s tem dodamo na konec urejenega dela).
 Postopek končamo, ko meja doseže konec tabele.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [swap a i j] zamenja elementa [a.(i)] and [a.(j)]. Zamenjavo naredi
 na mestu in vrne unit.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let test = [|0; 1; 2; 3; 4|];;
 val test : int array = [|0; 1; 2; 3; 4|]
 # swap test 1 4;;
 - : unit = ()
 # test;;
 - : int array = [|0; 4; 2; 3; 1|]
[*----------------------------------------------------------------------------*)

let swap a i j = 
    let ai = a.(i) in
    a.(i) <- a.(j); 
    a.(j) <- ai 

let swap a i j = 
    let ai = a.(i) in 
    let () = a.(i) <- a.(j) in 
    let () = a.(j) <- ai in 
    () 
 
(*----------------------------------------------------------------------------*]
 Funkcija [index_min a lower upper] poišče indeks najmanjšega elementa tabele
 [a] med indeksoma [lower] and [upper] (oba indeksa sta vključena).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 index_min [|0; 2; 9; 3; 6|] 2 4 = 3
[*----------------------------------------------------------------------------*)

let index_min a lower upper = 
    let rec search mini i = 
        if i > upper then 
            (*KOnčali iskanje*)
            mini 
        else if a.(i) < a.(mini) then 
            (*Našli nov najmanjši element, posodobi indeks minimuma.*)
            search i (i+1)
        else
            (*Nezannimiv element, išči naprej *)
            search mini (i+1)
    in
    (*začni iskanje pri [lower] kjer je najmanjši do sedaj videni element, 
    prav tako na indeksu [lower]*)
    search lower lower 

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort_array] implementira urejanje z izbiranjem na mestu. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Namig: Za testiranje uporabi funkciji [Array.of_list] in [Array.to_list]
 skupaj z [randlist].
[*----------------------------------------------------------------------------*)

(*
let selection_sort_array a = 
    let len = Array.length a in 
    let rec sorter lower = 
        if lower <= len then 
            (*everything is sorted!*)
            ()
        else
            (*find the minimal element in the rest of the list and swap it with the 
            element on the lower end of the 'to be sorted' part of the list. *)
            let mini = index_min a lower (len-1) in 
            let () = swap a lower mini in 
            (*sort the rest of the list*)
            sorter (lower+1)
    in 
    sorter 0 

let rec array_tester our_sorter len = 
    let test = randlist len 10000 in 
    let test_array = Array.of_list test in 
    let () = our_sort test_array in 
    Array.to_list test_array = List.sort compare test  
*)