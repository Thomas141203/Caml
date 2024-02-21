let rec maxPath t =
    match t with
    |E -> 0 
    |N(x,g,d) ->
         let p1 = maxPath g in
         let p2 = maxPath d in
        if p1 > p2 then x + p1
        else x + p2;;