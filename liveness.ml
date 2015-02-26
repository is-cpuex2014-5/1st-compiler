(* liveness analysis based on tiger book *
 * it works but a bug is left            *)

type t = {
  out : S.t M.t;
  def : S.t M.t;
  use  : S.t M.t;
}

(* operator to make a function as an infix operator *)
let (<|) x y = y x and (|>) x y = x y

(* used in gColoring.ml *)
let transfer stmt set =
  List.fold_left
    (fun acc x -> S.add x acc)
    (List.fold_left
       (fun acc (x, _) -> S.remove x acc)
       set
       stmt.Block.def)
    stmt.Block.use

(* zero registers should be always alive *)
let add_zero_reg liveness typ =
  let zreg = 
    match typ with
    | Type.Float -> "$f00"
    | _ -> "$r00"
  in 
  let liveout = 
    M.fold (fun x set acc -> M.add x (S.add zreg set) acc) 
	   liveness.out liveness.out 
  in
  { liveness with out = liveout }

(* !!!CRITICAL!!!!                                                                                    *
 * theoretically a threshold isn't needed, but somehow this algorithm doesn't stop with some programs *)
(* main function *) (* TODO: reduce side effect. use fold (or ...) instead of while *) 
let f cfg =
  let bs = try Block.CFG.top_sort cfg with _ -> Block.CFG.vertices cfg in 
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
  let count = ref 0 in
  while !flg && (!count < 100000) do
    let live_in' = !live_in in
    let live_out' = !live_out in
    count := !count + 1;
    flg := false;
    List.iter 
      (fun b ->
	 (* in[n] := use[n] ∪ (out[n] \ def[n]) *)
	 let blive_in = (assert (M.mem b use); M.find b use) <|S.union|> 
			  (assert (M.mem b !live_out);M.find b !live_out) <|S.diff|> (assert (M.mem b def); M.find b def)
	 in 
	 (* out[n] := ∪(forall s in succ[n]) in[s] *)
	 let blive_out = 
	   List.fold_left 
	     (fun env succ ->
	       env <|S.union|> (assert (M.mem succ !live_in); M.find succ !live_in))
	     S.empty (Block.CFG.succs b cfg)
	 in
	 live_in := M.add b blive_in !live_in;
	 live_out := M.add b blive_out !live_out;
	 flg := !flg || live_in' <> !live_in || live_out' <> !live_out)
      bs;

  done;
  if (!count = 100000)
  then 
    (*(M.iter (fun x y -> S.iter (fun z -> Printf.fprintf stderr "%s: %s " x z) y) !live_out;*) 
    (* assert false; *)   
     { out = !live_out;
      def = def;
      use  = use }
       
  else
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
