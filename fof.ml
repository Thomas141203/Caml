let rec fof h x =
    h (h x);;

let h x = x * x;;