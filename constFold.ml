open KNormal

let memi x env =
  try (match M.find x env with Int(_) -> true | _ -> false)
  with Not_found -> false
let memf x env =
  try (match M.find x env with Float(_) -> true | _ -> false)
  with Not_found -> false
let memt x env =
  try (match M.find x env with Tuple(_) -> true | _ -> false)
  with Not_found -> false
let memif x env =
  M.mem x env &&
    (match M.find x env with 
       IfEq (x, y, Int _, Int _) | IfLT (x, y, Int _, Int _) -> true 
       | _ -> false)

let findi x env = (match M.find x env with Int(i) -> i | _ -> raise Not_found)
let findf x env = (match M.find x env with Float(d) -> d | _ -> raise Not_found)
let findt x env = (match M.find x env with Tuple(ys) -> ys | _ -> raise Not_found)



let rec g env = function (* 定数畳み込みルーチン本体 (caml2html: constfold_g) *)
  | Var(x) when memi x env -> Int(findi x env)
  (* | Var(x) when memf x env -> Float(findf x env) *)
  (* | Var(x) when memt x env -> Tuple(findt x env) *)
  | Neg(x) when memi x env -> Int(-(findi x env))
  | Add(x, y) when memi x env && memi y env -> Int(findi x env + findi y env) (* 足し算のケース (caml2html: constfold_add) *)
  | Add(x, y) when memi x env && (findi x env) = 0 -> Var(y)
  | Add(x, y) when memi y env && (findi y env) = 0 -> Var(x)
  | Sub(x, y) when memi x env && memi y env -> Int(findi x env - findi y env)
  | Sub(x, y) when memi x env && (findi x env) = 0 -> Neg(y)
  | Sub(x, y) when memi y env && (findi y env) = 0 -> Var(x)
  | FNeg(x) when memf x env -> Float(-.(findf x env))
  | FAdd(x, y) when memf x env && memf y env -> Float(findf x env +. findf y env)
  | FAdd(x, y) when memf x env && (findf x env) = 0. -> Var(y)
  | FAdd(x, y) when memf y env && (findf y env) = 0. -> Var(x)
  | FSub(x, y) when memf x env && memf y env -> Float(findf x env -. findf y env)
  | FSub(x, y) when memf x env && (findf x env) = 0. -> FNeg(y)
  | FSub(x, y) when memf y env && (findf y env) = 0. -> Var(x)
  | FMul(x, y) when memf x env && memf y env -> Float(findf x env *. findf y env)
  | FMul(x, y) when memf x env && (findf x env) = 0. -> Float(0.)
  | FMul(x, y) when memf y env && (findf y env) = 0. -> Float(0.)
  | FMul(x, y) when memf x env && (findf x env) = 1. -> Var(y)
  | FMul(x, y) when memf y env && (findf y env) = 1. -> Var(x)
  | FMul(x, y) when memf x env && (findf x env) = -1. -> FNeg(y)
  | FMul(x, y) when memf y env && (findf y env) = -1. -> FNeg(x)
  | FDiv(x, y) when memf x env && memf y env -> Float(findf x env /. findf y env)
  | FDiv(x, y) when memf x env && (findf x env) = 0. -> Float(0.)
  | FDiv(x, y) when memf y env && (findf y env) = 1. -> Var(x)
  | FDiv(x, y) when memf y env && (findf y env) = -1. -> FNeg(x)
  | Ftoi(x) when memf x env -> Int(int_of_float(findf x env))
  | Itof(x) when memi x env -> Float(float_of_int(findi x env)) (*this might increases the number of instructions*)
  | IfEq(x, y, e1, e2) when memi x env && memi y env -> if findi x env = findi y env then g env e1 else g env e2
  | IfEq(x, y, e1, e2) when memf x env && memf y env -> if findf x env = findf y env then g env e1 else g env e2
  | IfLT(x, y, e1, e2) when memi x env && memi y env -> if findi x env < findi y env then g env e1 else g env e2
  | IfLT(x, y, e1, e2) when memf x env && memf y env -> if findf x env < findf y env then g env e1 else g env e2
  | IfEq (x, y, e1, e2) when memif x env && memi y env ->
     let i = findi y env in
     (match M.find x env with
      | IfEq (z, w, Int(i1), Int(i2)) when i1 = i && i2 = i -> g env e1
      | IfEq (z, w, Int(i1), Int(i2)) when i1 = i -> IfEq (z, w, g env e1, g env e2)
      | IfEq (z, w, Int(i1), Int(i2)) when i2 = i -> IfEq (z, w, g env e2, g env e1)
      | IfEq (z, w, Int(i1), Int(i2))  -> g env e2
      | IfLT (z, w, Int(i1), Int(i2)) when i1 = i && i2 = i -> g env e1
      | IfLT (z, w, Int(i1), Int(i2)) when i1 = i -> IfLT (z, w, g env e1, g env e2)
      | IfLT (z, w, Int(i1), Int(i2)) when i2 = i -> IfLT (z, w, g env e2, g env e1)
      | IfLT (z, w, Int(i1), Int(i2)) -> g env e2
      | _ -> assert false)
  | IfEq (y, x, e1, e2) when memif x env && memi y env ->
      let i = findi y env in
        (match M.find x env with
          | IfEq (z, w, Int(i1), Int(i2)) when i1 = i && i2 = i -> g env e1
          | IfEq (z, w, Int(i1), Int(i2)) when i1 = i -> IfEq (z, w, g env e1, g env e2)
          | IfEq (z, w, Int(i1), Int(i2)) when i2 = i -> IfEq (z, w, g env e2, g env e1)
          | IfEq (z, w, Int(i1), Int(i2)) -> g env e2
          | IfLT (z, w, Int(i1), Int(i2)) when i1 = i && i2 = i -> g env e2
          | IfLT (z, w, Int(i1), Int(i2)) when i1 = i -> IfLT (z, w, g env e1, g env e2)
          | IfLT (z, w, Int(i1), Int(i2)) when i2 = i -> IfLT (z, w, g env e2, g env e1)
          | IfLT (z, w, Int(i1), Int(i2)) -> g env e1
          | _ -> assert false)
  | IfLT (x, y, e1, e2) when memif x env && memi y env ->
      let i = findi y env in
        (match M.find x env with
          | IfEq (z, w, Int(i1), Int(i2)) when i1 < i && i2 < i -> g env e1
          | IfEq (z, w, Int(i1), Int(i2)) when i1 < i -> IfEq (z, w, g env e1, g env e2)
          | IfEq (z, w, Int(i1), Int(i2)) when i2 < i -> IfEq (z, w, g env e2, g env e1)
          | IfEq (z, w, Int(i1), Int(i2)) -> g env e2
          | IfLT (z, w, Int(i1), Int(i2)) when i1 < i && i2 < i -> g env e1
          | IfLT (z, w, Int(i1), Int(i2)) when i1 < i -> IfLT (z, w, g env e1, g env e2)
          | IfLT (z, w, Int(i1), Int(i2)) when i2 < i -> IfLT (z, w, g env e2, g env e1)
          | IfLT (z, w, Int(i1), Int(i2)) -> g env e2
          | _ -> assert false)
  | IfLT (y, x, e1, e2) when memif x env && memi y env ->
      let i = findi y env in
        (match M.find x env with
          | IfEq (z, w, Int(i1), Int(i2)) when i < i1 && i < i2 -> g env e1
          | IfEq (z, w, Int(i1), Int(i2)) when i < i1 -> IfEq (z, w, g env e1, g env e2)
          | IfEq (z, w, Int(i1), Int(i2)) when i < i2 -> IfEq (z, w, g env e2, g env e1)
          | IfEq (z, w, Int(i1), Int(i2)) -> g env e2
          | IfLT (z, w, Int(i1), Int(i2)) when i < i1 && i < i2 -> g env e1
          | IfLT (z, w, Int(i1), Int(i2)) when i < i1 -> IfLT (z, w, g env e1, g env e2)
          | IfLT (z, w, Int(i1), Int(i2)) when i < i2 -> IfLT (z, w, g env e2, g env e1)
          | IfLT (z, w, Int(i1), Int(i2)) -> g env e2
          | _ -> assert false)
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g env e1, g env e2)
  | Let((x, t), e1, e2) -> (* letのケース (caml2html: constfold_let) *)
      let e1' = g env e1 in
      let e2' = g (M.add x e1' env) e2 in
      Let((x, t), e1', e2')
  | LetRec({ name = x; args = ys; body = e1 }, e2) ->
      LetRec({ name = x; args = ys; body = g env e1 }, g env e2)
  | LetTuple(xts, y, e) when memt y env ->
      List.fold_left2
	(fun e' xt z -> Let(xt, Var(z), e'))
	(g env e)
	xts
	(findt y env)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
  | e -> e

let f = g M.empty
