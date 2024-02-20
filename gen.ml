let rec gen n x =
    if n = 0 then [] 
    else
       x::gen(n-1) x;;

gen 4 7;;
(* [7;7;7;7] *)