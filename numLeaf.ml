let rec nf t =
    match t with
    |E -> 1
    |N(x,g,d) -> (nf g) + (nf d);;