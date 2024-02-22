let rec divdiv2 x =
    if(x mod 2 != 0) then x
    else divdiv2 (x/2);;