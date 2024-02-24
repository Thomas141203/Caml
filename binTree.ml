type ntree = N of ntree list;;

type bintree = E | F | B of bintree * bintree;;

let rec n2bin t = 
    match t with
    |N[] -> F
    |N[u] -> B(n2bin u, E)
    |N(h::q) -> B(n2bin h, n2bin (N q))
;;

let rec bin2n t =
    match t with 
    |E -> failwith "Erreur"
    |F -> N[] 
    |B(g,E) -> N[bin2n g] 
    |B(g,d) -> 
        let t1 = bin2n g in
        let t2 = bin2n d in
        match t2 with
        |N[] -> failwith "Erreur"
        |N(h::q) -> N(t1::h::q)
;;

let tt = N[ N[N[];N[]]; N[N[]]; N[] ];;

n2bin tt;;
