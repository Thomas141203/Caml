let syr x = 
    if x mod 2 = 0 then
        x / 2
    else
        3 * x + 1 ;;

syr(syr 7);;


let mult a b = 
    if b mod a = 0 then
        true
    else 
        false;;


let rec sum w =
    match w with
    |[] -> 0
    |x::q -> x + (sum q);;


let rec gen n x =
    if n = 0 then [] 
    else
       x::gen(n-1) x;;


let rec d w = 
    match w with 
    |[] -> false
    |[x] -> false
    |a::b::q ->
        if a = b then true
        else 
            d (b::q);;


sigma longueur t;;

let rec fof h x =
    h (h x);;


type arbre = E of int |N of arbre*arbre;;
let a = N(N(E(3), E(5)), E(8));;

let rec sum a = 
    match a with
    | E(x) -> x
    |N(b, d) -> (sum b) + (sum d);;

let rec why x = 
    if x<=1 then 1
    else 
        x + why(x-1);;

let rec at w i =
    match w with
   |[] -> failwith "Erreur"
  |x::q ->
    if i = 0 then x
    else at q (i-1);;

let rec add w x =
    match w with
    |[] ->[x]
    |t::q -> t:: (add q x);;

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


let rec sz t = 
    match t with 
    |E -> 0
    |N(x,g,d) ->
        (sz g) + (sz d) + 1;;


let rec maxPath t =
    match t with
    |E -> 0 
    |N(x,g,d) ->
         let p1 = maxPath g in
         let p2 = maxPath d in
        if p1 > p2 then x + p1
        else x + p2;;


let rec collect

let rec collect t =
    match t with
    |F x -> [x]
    |N(g,d) -> (collect g) @ (collect d);;

let rec countb t =
    match t with
    |F -> 0
    |U f -> countb f
    |B(g,d) -> 1 + (countb g) + (countb d);;

let rec lmax t =
    match t with
    |F -> 0
    |U f -> (lmax f) + 1
    |B(g,d) ->
        let m1 = lmax d in
        let m2 = lmax g in
        if m1>m2 then m1+1
        else m2+1;;

let rec divdiv2 x =
    if(x mod 2 != 0) then x
    else divdiv2 (x/2);;

let rec gen a b =
  if a = b then [a]
  else a :: gen(a+1) b;;

let rec mirroir w
    match w with
    |[] -> []
    |x::q -> (mirroir q) @ [x];;

let rec concat a b =
  match a with
  |[] -> b
  |ta::qa -> ta (concat qa b);;

let rec nf t =
    match t with
    |E -> 1
    |N(x,g,d) -> (nf g) + (nf d);;

let rec minLeaf t =
    match t with
    |F x -> x
    |N(g,d) -> 
        let m1 = minLeaf g in
        let m2 = minLeaf d in
        if m1 > m2 then m2
        else m1;;

let rec prefx t = 
    match t with 
    |E -> []
    |N(x, g, d) -> x:: (prefx g) @ (prefx d);;

let rec sumt tr =
    match tr with
    |L(x,[]) -> x
    |L(x,t::q) ->
        let s1 = sumt t in
        let s2 = sumt (L(0,q)) in
        s1 + s2 + x;;
