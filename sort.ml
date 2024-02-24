(*
exemple : tri [17;5;1;45;2;1;3;24];;   ---> [1;1;2;3;5;17;24;45]

Le tri réalisé doit être basé sur le principe suivant :

1. Chaque élément de la liste à trier est inséré dans un arbre binaire trié.
2. L’arbre trié est converti e n liste.


Un arbre binaire trié est un arbre binaire dont les noeuds sont étiquetés par des entiers, dont les feuilles n’ont pas d’étiquette, et qui a les propriété suivantes.

- Toutes les étiquettes du fils gauche d’un arbre trié sont au plus égales à l’étiquette de sa racine.
- Toutes les étiquettes de son fils droit sont supérieures à l’étiquette de sa racine.
- Les sous arbres d’un arbre binaire trié sont des arbres binaires triés.

*)

type stree = L | N of int * stree * stree;;

let rec insert x t =
    match t with
    |L -> N(x,L,L)
    |N(y,g,d) ->
        if x<=y then N(y,(insert x g),d)
        else N(y,g,(insert x d))
;;
  
let rec list2tree w =
    match w with
    |[] -> L
    |x::q -> insert x (list2tree q)
;;

let rec tree2list t =
    match t with
    |L -> []
    |N(x,g,d) -> 
        (tree2list g) @ x::(tree2list d)
;;

let tri w =
    tree2list (list2tree w)
;;

tri [17;5;1;45;2;1;3;24];;