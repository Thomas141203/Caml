let rec sumt tr =
    match tr with
    |L(x,[]) -> x
    |L(x,t::q) ->
        let s1 = sumt t in
        let s2 = sumt (L(0,q)) in
        s1 + s2 + x;;
