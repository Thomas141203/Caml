let rec why x = 
    if x<=1 then 1
    else 
        x + why(x-1);;