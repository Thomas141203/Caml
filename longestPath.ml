let rec lmax t =
    match t with
    |F -> 0
    |U f -> (lmax f) + 1
    |B(g,d) ->
        let m1 = lmax d in
        let m2 = lmax g in
        if m1>m2 then m1+1
        else m2+1;;