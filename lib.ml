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
  let xx = x *. x in
  x *. (1.0 +. xx *. (-0.16666669 +. xx *. (0.008332824 +. xx *. -0.00019587841)))
in

let rec kernel_cos x = 
  let xx = x *. x in
  1.0 +. xx *. (-0.5 +. xx *. (0.04166368 +. xx *. -0.0013695068))
in

let rec sin_sub x flag = 
let pi = 3.14159265358979323846264 in
let pih = pi *. 0.5 in
let piq = pi *. 0.25 in
  let res = 
    if x >= pi then 
      sin_sub (x -. pi)  (-. flag)
    else if x >= pih then 
      sin_sub (pi -. x) flag
    else if x <= piq then
      flag *. (kernel_sin x)
    else 
      let y = pih -. x in
      flag *. kernel_cos y
  in 
    res 
in
      
let rec cos_sub x flag =
let pi = 3.14159265358979323846264 in
let pih = pi *. 0.5 in
let piq = pi *. 0.25 in
  let res = 
    if x >= pi then 
      cos_sub (x -. pi) (-. flag)
    else if x >= pih then
      cos_sub (pi -. x) (-. flag)
    else if x <= piq then
	flag *. kernel_cos x
    else 
	let y = pih -. x in
	flag *. kernel_sin y
  in
    res 
in

let rec sin x = 
let pi = 3.14159265358979323846264 in
let pi2 = pi *. 2.0 in
  if pi2 <= x then
    sin (x -. pi2)
  else if 0. < x then 
    sin_sub x 1.
  else if (-. pi2) < x then
    sin_sub (-. x) (-. 1.)
  else
    sin (x +. pi2)
in
 
let rec cos x = 
let pi = 3.14159265358979323846264 in
let pi2 = pi *. 2.0 in
  if pi2 <= x then
    cos (x -. pi2)
  else if 0. < x then 
    cos_sub x 1.
  else if -. pi2 < x then
    cos_sub (-. x) 1.
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

