let count = ref 0;;

let rec min3 w =
    (
        count := !count + 1
        match w with
        |[] -> failwith "Erreur"
        |[x] -> x
        |t::q -> 
            if t <  min3 q then t else min3 q
    );;

min3 [6;5;4;3;2;1];;
!count;;