let rec add w x =
    match w with
    |[] ->[x]
    |t::q -> t:: (add q x);;
