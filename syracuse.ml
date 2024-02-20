let syr x = 
    if x mod 2 = 0 then
        x / 2
    else
        3 * x + 1 ;;

syr(syr 7);;