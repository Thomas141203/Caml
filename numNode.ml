let rec sz t = 
    match t with 
    |E -> 0
    |N(x,g,d) ->
        (sz g) + (sz d) + 1;;