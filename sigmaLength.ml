let rec longueur w =
    match w with
    |[] -> 0
    |x::q -> 1 + longueur q;;

let t = [[1;2];[7];[3;5;1]];;

sigma longueur t;;