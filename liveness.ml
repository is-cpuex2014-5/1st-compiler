type t = {
  out : S.t M.t;
  def : S.t M.t;
  use  : S.t M.t;
}


(* operator to make a function as an infix operator *)
let (<|) x y = y x and (|>) x y = x y

let f cfg =
  let bs = Block.CFG.top_sort cfg in 
  let live_in = ref (List.fold_right (fun b m -> M.add b S.empty m) bs M.empty) in
  let live_out = ref !live_in in
  let def =
    Block.CFG.fold
      (fun b stmts acc ->
       M.add b
             (List.fold_left
		(fun def_set { Block.def = defs } -> 
                 List.fold_right (fun (x, _) -> S.add x) defs def_set)
		S.empty 
		stmts)
             acc)
      cfg M.empty
  in
  
  let use =
    Block.CFG.fold
      (fun b stmts acc ->
         M.add b
           (fst
              (List.fold_left
                (fun (use_set, env) { Block.use = uses; Block.def = defs } ->
                 let use_set = ((S.of_list uses) <|S.diff|> env) <|S.union|> use_set in
                   let env = List.fold_right (fun (x, _) -> S.add x) defs env in
                     (use_set, env))
                (S.empty, S.empty) stmts))
           acc)
      cfg M.empty
  in

  let flg = ref true in
  while !flg do
    let live_in' = !live_in in
    let live_out' = !live_out in
    flg := false;
    List.iter 
      (fun b ->
	 let blive_in' = (assert (M.mem b !live_in); M.find b !live_in) in 
	 let blive_out' = (assert (M.mem b !live_out); M.find b !live_out) in
	 (* in[n] <- use[n] ∪ (out[n] - def[n]) *)
	 let blive_in = (assert (M.mem b use); M.find b use) <|S.union|> 
			  (M.find b !live_out) <|S.diff|> (assert (M.mem b def); M.find b def)
	 in 
	 (* out[n] <- ∪(forall s in succ[n]) in[s] *)
	 let blive_out = 
	   List.fold_left (
	       fun env succ ->
	       env <|S.union|> (assert (M.mem succ !live_in); M.find succ !live_in)
	     ) S.empty (Block.CFG.succs b cfg)
	 in
	 live_in := M.add b blive_in !live_in;
	 live_out := M.add b blive_out !live_out;
	 flg := !flg || live_in' <> !live_in || live_out' <> !live_out)
      bs;

  done;
    { out = !live_out;
      def = def;
      use  = use }
      
(* for debug *)
let string_of_liveness { out = out; def = def; use = use } = 
  "***** live out *****\n\n" ^ 
    (M.fold  (fun b s str -> b ^ ":\n" ^ (S.fold (fun x y -> x ^ " " ^ y) s "") ^ "\n" ^ str) out "") ^
      "\n***** def *****\n\n" ^
	(M.fold  (fun b s str -> b ^ ":\n" ^ (S.fold (fun x y -> x ^ " " ^ y) s "") ^ "\n" ^ str) def "") ^
	"\n***** use *****\n\n" ^
	  (M.fold  (fun b s str -> b ^ ":\n" ^ (S.fold (fun x y -> x ^ " " ^ y) s "") ^ "\n" ^ str) use "") ^
	  "\n"

let print_liveness oc x = 
  Printf.fprintf oc "%s\n" (string_of_liveness x)
