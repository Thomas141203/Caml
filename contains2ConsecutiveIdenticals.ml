let rec d w = 
    match w with 
    |[] -> false
    |[x] -> false
    |a::b::q ->
        if a = b then true
        else 
            d (b::q);;