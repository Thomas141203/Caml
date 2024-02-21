let rec countb t =
    match t with
    |F -> 0
    |U f -> countb f
    |B(g,d) -> 1 + (countb g) + (countb d);;