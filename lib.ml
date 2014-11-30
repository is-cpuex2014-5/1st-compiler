let rec fiszero x = x = 0. in
let rec fispos x = x > 0. in
let rec fisneg x = x < 0. in
let rec fequal x y = x = y in
let rec fless x y = x < y in
let rec fneg x = -.x in
let rec fabs x = if x < 0. then -.x else x in
let rec fsqr x = x *. x in 
let rec fhalf x = x /. 2. in

let rec kernel_sin x =
  let res = x in
  let res = res -. (0.16666668 *. x *. x *. x) in
  let res = res +. (0.008332824 *. x *. x *. x *. x *. x) in
  let res = res -. (0.00019587841 *. x *. x *. x *. x *. x *. x *. x) in
  res in

let rec kernel_cos x = 
  let res = 1.0 in
  let res = res -. (0.5 *. x *. x) in
  let res = res +. (0.04166368 *. x *. x *. x *.x) in
  let res = res -. (0.0013695068 *. x *. x *. x *. x *. x *. x) in
  res 
in
let rec sin_sub x flag = 
let pi = 3.14159265358979323846264 in
let pi2 = pi *. 2.0 in
let pih = pi *. 0.5 in
let piq = pi *. 0.25 in
  let res = 
    if x >= pi then 
	sin_sub (x -. pi) (- flag)
    else if x >= pih then
        let flag = - flag in 
	if x <= piq then
	  kernel_sin x
	else 
	  let y = pih -. x in
	  kernel_cos y
    else if x <= piq then
	kernel_sin x
    else 
	let y = pih -. x in
	kernel_cos y
  in
  if flag = 1 then 
    -. res
  else 
    res 
in      
let rec cos_sub x flag =
let pi = 3.14159265358979323846264 in
let pi2 = pi *. 2.0 in
let pih = pi *. 0.5 in
let piq = pi *. 0.25 in
  let res = 
    if x >= pi then 
	sin_sub (x -. pi) (- flag)
    else if x >= pih then
      let flag = - flag in
      if x <= piq then
	kernel_cos x
      else 
	let y = pih -. x in
	kernel_sin y
    else if x <= piq then
	kernel_cos x
    else 
	let y = pih -. x in
	kernel_sin y
  in
  if flag = 1 then 
    -. res
  else 
    res 
in
let rec sin x = 
let pi = 3.14159265358979323846264 in
let pi2 = pi *. 2.0 in
  if pi2 <= x then
    sin (x -. pi2)
  else if 0. < x then 
    sin_sub x (-1)
  else if -. pi2 < x then
    sin_sub x 1
  else
    sin (x +. pi2)
in
let rec cos x = 
let pi = 3.14159265358979323846264 in
let pi2 = pi *. 2.0 in
  if pi2 <= x then
    cos (x -. pi2)
  else if 0. < x then 
    cos_sub x (-1)
  else if -. pi2 < x then
    cos_sub x 1
  else
    cos (x +. pi2)
in

let rec atan x = 
  let res = 0.0 in
  let res = res *. x +. 0.0296672 in
  let res = res *. x -. 0.139238 in
  let res = res *. x -. 0.0174956 in
  let res = res *. x +. 0.928238 in
  let res = res *. x -. 0.0149898 in
    res 
in

let rec floor x = 
  let y = int_of_float(x) in
  if x > 0.0 then
    float_of_int(y)
  else 
    if float_of_int(y) = x then
      x
    else
      let y = y - 1 in
      float_of_int(y)
in			      

