let rec merge_sort lst =
    let rec merge left right =
        match left, right with
            | [], right -> right
            | left, [] -> left
            | h1 :: t1, h2 :: t2 ->
            if h1 < h2 then h1 :: merge t1 right
            else h2 :: merge left t2 in
    
    let rec split = function
        | [] -> [], []
        | [x] -> [x], []
        | x :: y :: rest -> 
            let left, right = split rest in x :: left, y :: right in

            match lst with
                | [] -> []
                | [x] -> [x]
                | _ ->
                    let left, right = split lst in merge (merge_sort left) (merge_sort right)

let nums = [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5]
let sorted_nums = merge_sort nums

