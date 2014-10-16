(* give names to intermediate values (K-normalization) *)

type t = (* K正規化後の式 (caml2html: knormal_t) *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 (caml2html: knormal_branch) *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtTuple of Id.t
  | ExtFunApp of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec fv = function (* 式に出現する（自由な）変数 (caml2html: knormal_fv) *)
  | Unit | Int(_) | Float(_) | ExtArray(_) | ExtTuple(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))

let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let e', t' = k x in
      Let((x, t), e, e'), t'

let rec g env = function (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  | Syntax.Unit -> Unit, Type.Unit
  | Syntax.Bool(b) -> Int(if b then 1 else 0), Type.Int (* 論理値true, falseを整数1, 0に変換 (caml2html: knormal_bool) *)
  | Syntax.Int(i) -> Int(i), Type.Int
  | Syntax.Float(d) -> Float(d), Type.Float
  | Syntax.Not(e) -> g env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
  | Syntax.Neg(e) ->
      insert_let (g env e)
	(fun x -> Neg(x), Type.Int)
  | Syntax.Add(e1, e2) -> (* 足し算のK正規化 (caml2html: knormal_add) *)
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> Add(x, y), Type.Int))
  | Syntax.Sub(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> Sub(x, y), Type.Int))
  | Syntax.FNeg(e) ->
      insert_let (g env e)
	(fun x -> FNeg(x), Type.Float)
  | Syntax.FAdd(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> FAdd(x, y), Type.Float))
  | Syntax.FSub(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> FSub(x, y), Type.Float))
  | Syntax.FMul(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> FMul(x, y), Type.Float))
  | Syntax.FDiv(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> FDiv(x, y), Type.Float))
  | Syntax.Eq _ | Syntax.LE _ as cmp ->
      g env (Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
  | Syntax.If(e1, e2,e3) -> 
     let e1' = Syntax.getexp e1 in
     (match Syntax.If(e1', e2, e3) with
      | Syntax.If(Syntax.Not(e1''), e2', e3') -> g env (Syntax.If(e1'', e3', e2')) (* notによる分岐を変換 (caml2html: knormal_not) *)
      | Syntax.If(Syntax.Eq(e1'', e2'), e3', e4') ->
	 insert_let (g env e1'')
		    (fun x -> insert_let (g env e2')
					 (fun y ->
					  let e3'', t3 = g env e3' in
					  let e4'', t4 = g env e4' in
					  IfEq(x, y, e3'', e4''), t3))
      | Syntax.If(Syntax.LE(e1'', e2'), e3', e4') ->
	 insert_let (g env e1'')
		    (fun x -> insert_let (g env e2')
					 (fun y ->
					  let e3'', t3 = g env e3' in
					  let e4'', t4 = g env e4' in
					  IfLE(x, y, e3'', e4''), t3))
      | _ -> g env (Syntax.If(Syntax.Eq(e1', Syntax.Bool(false)), e3, e2))) (* 比較のない分岐を変換 (caml2html: knormal_if) *)
  | Syntax.Let((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
      Let((x, t), e1', e2'), t2
  | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env
  | Syntax.Var(x) -> (* 外部配列及び外部タプルの参照 (caml2html: knormal_extarray) *)
      (match M.find x !Typing.extenv with
      | Type.Array(_) as t -> ExtArray x, t
      | Type.Tuple(_) as t -> ExtTuple x, t
      | t' -> failwith (Printf.sprintf "external variable %s does not have an array type but type %s" x (Type.sprint t')))
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (M.add_list yts env') e1 in
      LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
  | Syntax.App(e1, e2s) -> 
     let e3 = Syntax.getexp e1 in
     (match Syntax.App(e3, e2s)  with
      | Syntax.App(Syntax.Var(f), e2s) when not (M.mem f env) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
	 (match M.find f !Typing.extenv with
	  | Type.Fun(_, t) ->
	     let rec bind xs = function (* "xs" are identifiers for the arguments *)
	       | [] -> ExtFunApp(f, xs), t
	       | e2 :: e2s ->
		  insert_let (g env e2)
			     (fun x -> bind (xs @ [x]) e2s) in
	     bind [] e2s (* left-to-right evaluation *)
	  | _ -> assert false)
      | _ -> (match g env e3 with
	      | _, Type.Fun(_, t) as g_e1 ->
		 insert_let g_e1
			    (fun f ->
			     let rec bind xs = function (* "xs" are identifiers for the arguments *)
		| [] -> App(f, xs), t
		| e2 :: e2s ->
		   insert_let (g env e2)
			      (fun x -> bind (xs @ [x]) e2s) in
			     bind [] e2s) (* left-to-right evaluation *)
	      | _ -> assert false))
  | Syntax.Tuple(es) ->
      let rec bind xs ts = function (* "xs" and "ts" are identifiers and types for the elements *)
	| [] -> Tuple(xs), Type.Tuple(ts)
	| e :: es ->
	    let _, t as g_e = g env e in
	    insert_let g_e
	      (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
      bind [] [] es
  | Syntax.LetTuple(xts, e1, e2) ->
      insert_let (g env e1)
	(fun y ->
	  let e2', t2 = g (M.add_list xts env) e2 in
	  LetTuple(xts, y, e2'), t2)
  | Syntax.Array(e1, e2) ->
      insert_let (g env e1)
	(fun x ->
	  let _, t2 as g_e2 = g env e2 in
	  insert_let g_e2
	    (fun y ->
	      let l =
		match t2 with
		| Type.Float -> "create_float_array"
		| _ -> "create_array" in
	      ExtFunApp(l, [x; y]), Type.Array(t2)))
  | Syntax.Get(e1, e2) ->
      (match g env e1 with
      |	_, Type.Array(t) as g_e1 ->
	  insert_let g_e1
	    (fun x -> insert_let (g env e2)
		(fun y -> Get(x, y), t))
      | _ -> assert false)
  | Syntax.Put(e1, e2, e3) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> insert_let (g env e3)
		(fun z -> Put(x, y, z), Type.Unit)))
  | Syntax.Pos (_, e1) -> g env e1

let f e = fst (g M.empty e)

let rec indent oc =function
  | 0 -> ()
  | n -> Printf.fprintf oc "  "; indent oc (n-1)

let pid oc n s= indent oc n; Printf.fprintf oc "ID %s\n" s
let pname oc fdef =  let (s, t) = fdef.name in
		     Printf.fprintf oc "NAME %s : " s; Type.p oc t; Printf.fprintf oc " \n"
let pargs oc fdef =  Printf.fprintf oc "ARGS "; 
		     List.iter (fun (x,y) -> Printf.fprintf oc " %s : " x; Type.p oc y) fdef.args; Printf.fprintf oc " \n"

let p oc e = 
  let rec f n e' = 
    let rec g n = function
      | Unit -> Printf.fprintf oc "UNIT\n"
      | Int i -> Printf.fprintf oc "INT %d\n" i
      | Float f -> Printf.fprintf oc "FLOAT %f\n" f
      | Neg s -> Printf.fprintf oc "NEG\n"; 
		 pid oc (n+1) s
      | Add (s1, s2) -> Printf.fprintf oc "ADD\n";
			pid oc (n+1) s1; pid oc (n+1) s2
      | Sub (s1, s2) -> Printf.fprintf oc "SUB\n"; 
			pid oc (n+1) s1; pid oc (n+1) s2
      | FNeg s ->  Printf.fprintf oc "FNEG\n"; 
		   pid oc (n+1)  s
      | FAdd (s1, s2) -> Printf.fprintf oc "FADD\n";
			 pid oc (n+1) s1; pid oc (n+1) s2
      | FSub (s1, s2) -> Printf.fprintf oc "FSUB\n";
			 pid oc (n+1) s1; pid oc (n+1) s2
      | FMul (s1, s2) -> Printf.fprintf oc "FMUL\n";
			 pid oc (n+1) s1; pid oc (n+1) s2
      | FDiv (s1, s2) -> Printf.fprintf oc "FDIV\n";
			 pid oc (n+1) s1; pid oc (n+1) s2
      | IfEq (s1, s2, e1, e2) -> Printf.fprintf oc "IFEQ\n";
				 pid oc (n+1) s1; pid oc (n+1) s2;
				 f (n+1) e1; f (n+1) e2
      | IfLE (s1, s2, e1, e2) -> Printf.fprintf oc "IFLE\n";
				 pid oc (n+1) s1; pid oc (n+1) s2;
				 f (n+1) e1; f (n+1) e2
      | Let ((s, t), e1, e2) ->  Printf.fprintf oc "LET %s : " s; Type.p oc t; Printf.fprintf oc " \n";
				 f (n+1) e1; f (n+1) e2
      | Var s -> Printf.fprintf oc "VAR %s\n" s
      | LetRec (fdef, s1) -> Printf.fprintf oc "LETREC\n";
			     indent oc (n+1); pname oc fdef;
			     indent oc (n+1); pargs oc fdef;
			     indent oc (n+1); Printf.fprintf oc "BODY\n"; f (n+2) fdef.body;
			     f (n+1) s1
      | App (s1, l) -> Printf.fprintf oc "APP\n";  pid oc (n+1) s1; List.iter (pid oc (n+1)) l 
      | Tuple l ->  Printf.fprintf oc "TUPLE\n"; List.iter (pid oc (n+1)) l       
      | LetTuple (l, s1, e1) -> Printf.fprintf oc "LETTUPLE ";List.iter (fun (x,y) -> Printf.fprintf oc " %s : " x; Type.p oc y) l; Printf.fprintf oc " \n";
				pid oc (n+1) s1; f (n+1) e1
      | Get (s1, s2) -> Printf.fprintf oc "GET\n"; pid oc (n+1) s1; pid oc (n+1) s2
      | Put (s1, s2, s3) ->  Printf.fprintf oc "PUT\n"; pid oc (n+1) s1; pid oc (n+1) s2; pid oc (n+1) s3
      | ExtArray s ->  Printf.fprintf oc "EXTARRAY\n"; pid oc (n+1) s
      | ExtTuple s ->  Printf.fprintf oc "EXTTUPLE\n"; pid oc (n+1) s 					   
      | ExtFunApp (s, l) ->  Printf.fprintf oc "EXTFUNAPP\n"; pid oc (n+1) s;
			     List.iter (pid oc (n+1)) l
    in 
    indent oc n; g n e'
  in f 0 e; e
