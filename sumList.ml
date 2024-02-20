let rec sum w =
    match w with
    |[] -> 0
    |x::q -> x + (sum q);;