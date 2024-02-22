let rec prefx t = 
    match t with 
    |E -> []
    |N(x, g, d) -> x:: (prefx g) @ (prefx d);;
