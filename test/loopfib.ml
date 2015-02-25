let rec fib n = 
  let rec fibi m n l = 
    if m = 0 then l 
    else fibi (m - 1) l (l + n)
  in fibi n 0 1
    in fib 10
  
