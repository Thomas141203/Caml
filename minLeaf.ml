let rec minLeaf t =
    match t with
    |F x -> x
    |N(g,d) -> 
        let m1 = minLeaf g in
        let m2 = minLeaf d in
        if m1 > m2 then m2
        else m1;;