let rec mirroir w
    match w with
    |[] -> []
    |x::q -> (mirroir q) @ [x];;