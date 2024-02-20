let rec min w =
    match w with
    |[] -> failwith "Erreur"
    |[x] -> x
    |t::q -> let m = min q in
        if t < m then t else m;;


let rec min w = 
    match w with
    |[] -> failwith "Erreur"
    |[x] -> x
    |t::q -> 
        if t <  min q then t else min q;;

(* O(2^n) *)