let rec at w i =
    match w with
    |[] -> failwith "Erreur"
    |x::q ->
        if i = 0 then x
        else at q (i-1);;