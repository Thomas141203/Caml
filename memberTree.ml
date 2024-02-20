let rec member x t =
    match t with
    |E -> false
    |N(y, left, right) ->
        if x = y then true
        else if member x left then true
        else member x right;;