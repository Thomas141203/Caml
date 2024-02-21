let rec collect t =
    match t with
    |F x -> [x]
    |N(g,d) -> (collect g) @ (collect d);;