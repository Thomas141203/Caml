let rec pref p w = 
    match p with
    |[] -> true
    |tp::qp -> 
        match w with
        |[] -> false
        |tw::qw ->
            if tw = tp then 
                pref qp qw
            else 
                false;;