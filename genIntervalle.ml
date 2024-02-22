let rec gen a b =
    if a = b then [a]
    else a :: gen(a+1) b;;