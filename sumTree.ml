type arbre = E of int |N of arbre*arbre;;
let a = N(N(E(3), E(5)), E(8));;

let rec sum a = 
    match a with
    |E(x) -> x
    |N(b, d) -> (sum b) + (sum d);;