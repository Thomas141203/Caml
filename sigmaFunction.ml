let rec sigma f w =
    match w with
    |[] -> 0
    |x::q -> f x + sigma f q;;

let h x = x * x;;

sigma h [1;2;3];;