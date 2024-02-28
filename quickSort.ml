let rec quicksort = function
    | [] -> []
    | pivot :: rest ->
        let lesser, greater = List.partition (fun x -> x < pivot) rest in quicksort lesser @ [pivot] @ quicksort greater

let nums = [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5]
let sorted_nums = quicksort nums