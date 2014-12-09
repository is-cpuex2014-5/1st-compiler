(* translation into RICE  assembly (infinite number of virtual registers) *)

open Asm

let data = ref [] (* 浮動小数点数の定数テーブル *)

let classify xts ini addf addi =
  List.fold_left
    (fun acc (x, t) -> match t with
       | Type.Unit -> acc
       | Type.Float -> addf acc x
       | _ -> addi acc x t) ini xts

let separate xts = 
  classify 
    xts 
    ([], []) 
    (fun (int, float) x -> (int, float @ [x]))
    (fun (int, float) x _ -> (int @ [x], float))

let expand xts ini addf addi = 
  classify
    xts
    ini
    (fun (offset, acc) x -> (offset + 4, addf x offset acc))
    (fun (offset, acc) x t -> (offset + 4, addi x t offset acc))

let rec g env = function (* 式の仮想マシンコード生成 *)
  | Closure.Unit -> Ans (Nop)
  | Closure.Int (i) -> Ans (Li (i))
  | Closure.Float (d) -> 
      let l = 
	try
	  let (l, _) = List.find (fun (_, d') -> d = d') !data in
	    l
	with Not_found ->
	  let l = (if d = 0.0 then Id.L ("fzero") else Id.L (Id.genid "l")) in
	    data := (l, d) :: !data;
	    l in
	Ans (FLi (l))
  | Closure.Neg (x) -> Ans (Neg (x))
  | Closure.Add (x, y) -> Ans (Add (x, V(y))) 
  | Closure.Sub (x, y) -> Ans (Sub (x, V(y)))
  | Closure.Shift(x,i) -> if i >= 0 then
			    Ans (Sll(x, C(i)))
			  else 
			    Ans (Srl(x, C(-i)))
  | Closure.FNeg (x) -> Ans (FNeg (x))
  | Closure.FAdd (x, y) -> Ans (FAdd (x, y))
  | Closure.FSub (x, y) -> Ans (FSub (x, y))
  | Closure.FMul (x, y) -> Ans (FMul (x, y))
  | Closure.FDiv (x, y) -> Ans (FDiv (x, y))
  | Closure.Itof (x) -> Ans (Itof (x))
  | Closure.Ftoi (x) -> Ans (Ftoi (x))
  | Closure.IfEq (x, y, e1, e2) -> 
      (match M.find x env with
	 | Type.Bool | Type.Int -> Ans (IfEq (x, y, g env e1, g env e2))
	 | Type.Float -> Ans (IfFEq (x, y, g env e1, g env e2))
	 | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLT (x, y, e1, e2) ->
      (match M.find x env with
	 | Type.Bool | Type.Int -> Ans (IfLT (x, y, g env e1, g env e2))
	 | Type.Float -> Ans (IfFLT (x, y, g env e1, g env e2))
	 | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.Let ((x, t1), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x t1 env) e2 in
	concat e1' (x, t1) e2'
  | Closure.Var (x) ->
      (match M.find x env with
	 | Type.Unit -> Ans (Nop)
	 | Type.Float -> Ans (FMov (x))
	 | _ -> Ans (Mov (x)))
  | Closure.MakeCls ((x, t), {Closure.entry = l; Closure.actual_fv = ys}, e2) ->
      (* closure のアドレスをセットしてからストア *)
      let e2' = g (M.add x t env) e2 in
      let (offset, store_fv) = 
	expand
	  (List.map (fun y -> (y, M.find y env)) ys)
	  (4, e2')
	  (fun y offset store_fv -> seq (FStore (y, x, C (offset)), store_fv))
	  (fun y _ offset store_fv -> seq (Store (y, x, C (offset)), store_fv)) in
	Let ((x, t), Mov (reg_hp), 
	     Let ((reg_hp, Type.Int), Add (reg_hp, C (align offset)), 
	     let z = Id.genid "l" in  
	       Let ((z, Type.Int), SetL(l), 
		       seq (Store (z, x, C (0)), store_fv))))
  | Closure.AppCls (x, ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
	Ans (CallCls (x, int, float))
  | Closure.AppDir(Id.L(x), (y :: _ as ys)) when x = "print_char" && List.length ys = 1 ->
     Ans (Write(y))
  | Closure.AppDir(Id.L(x), (y1 :: y2 :: _ as ys)) when x = "xor" && List.length ys = 2 ->
     Ans (Xor(y1, y2))
  | Closure.AppDir(Id.L(x), (y :: _ as ys)) when x = "sqrt" && List.length ys = 1 ->
     if !Asm.sqrtflag then
       Ans (FSqrt(y))  
     else
       Ans (CallDir (Id.L "sqrt", [], [y]))
  | Closure.AppDir(Id.L(x), (y :: _ as ys)) when x = "finv" && List.length ys = 1 ->
     if !Asm.invflag then
       Ans (FInv(y)) 
     else
       Ans (CallDir (Id.L "inv", [], [y]))
  | Closure.AppDir (Id.L(x), ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
	Ans (CallDir (Id.L(x), int, float))
  | Closure.Tuple (xs) -> (* 組の生成 *)
      let y = Id.genid "t" in
      let (offset, store) = 
	expand
	  (List.map (fun x -> (x, M.find x env)) xs)
	  (0, Ans (Mov (y)))
	  (fun x offset store -> seq (FStore (x, y, C (offset)), store))
	  (fun x _ offset store -> seq (Store (x, y, C (offset)), store))  in
	Let ((y, Type.Tuple (List.map (fun x -> M.find x env) xs)), Mov (reg_hp),
	     Let ((reg_hp, Type.Int), Add (reg_hp, C (align offset)), store))
  | Closure.LetTuple (xts, y, e2) -> 
      let s = Closure.fv e2 in
      let (offset, load) = 
	expand
	  xts
	  (0, g (M.add_list xts env) e2)
	  (fun x offset load ->
	     if not (S.mem x s) then load 
	     else fletd (x, FLoad (y, C (offset)), load))
	  (fun x t offset load ->
	     if not (S.mem x s) then load 
	     else Let ((x, t), Load (y, C (offset)), load)) in
	load
  | Closure.Get (x, y) -> (* 配列の読み出し *) 
      let offset = Id.genid "o" in  
	(match M.find x env with
	   | Type.Array (Type.Unit) -> Ans (Nop)
	   | Type.Array (Type.Float) ->
	       Let ((offset, Type.Int), Sll (y, C (2)),
		    Ans (FLoad (x, V (offset))))
	   | Type.Array (_) ->
	       Let ((offset, Type.Int), Sll (y, C (2)),
		    Ans (Load (x, V (offset))))

	   | _ -> assert false)
  | Closure.Put (x, y, z) ->
      let offset = Id.genid "o" in 
	(match M.find x env with
	   | Type.Array (Type.Unit) -> Ans (Nop)
	   | Type.Array (Type.Float) ->
	       Let ((offset, Type.Int), Sll (y, C (2)), 
		    Ans (FStore (z, x, V (offset)))) 
	   | Type.Array (_) ->
	       Let ((offset, Type.Int), Sll (y, C (2)), 
		    Ans (Store (z, x, V (offset)))) 
	   | _ -> assert false)
  | Closure.ExtArray (Id.L(x)) -> Ans(SetL(Id.L(x)))
  | Closure.ExtTuple (Id.L(x)) -> Ans(SetL(Id.L(x)))

(* 関数の仮想マシンコード生成 *)
let h { Closure.name = (Id.L(x), t); Closure.args = yts; 
	Closure.formal_fv = zts; Closure.body = e} =
  let (int, float) = separate yts in
  let (offset, load) = 
    expand
      zts
      (4, g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
      (fun z offset load -> fletd (z, FLoad (reg_cl, C (offset)), load))
      (fun z t offset load -> Let ((z, t), Load (reg_cl, C (offset)), load)) in
    match t with
      | Type.Fun (_, t2) ->
	  { name = Id.L(x); args = int; fargs = float; body = load; ret = t2 }
      | _ -> assert false

(* プログラム全体の仮想マシンコード生成 *)
let f (Closure.Prog (fundefs, e)) =
  data := [];
  let fundefs = List.map h fundefs in
  let e = g M.empty e in
    Prog (!data, fundefs, e)
