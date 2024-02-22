let rec concat a b =
    match a with
    |[] -> b
    |ta::qa -> ta (concat qa b);;