(* additional rules for regAlloc. most of the allocation is done in gColoring.ml *)
(* TODO : write more comments. refactor the code *)


(* save before call and restore after call *)
let save_and_restore  func =
  let cfg = func.Block.blocks in
  let liveness = Liveness.f cfg in
  let cfg =
    Block.CFG.map
      (fun b stmts ->
         snd
           (List.fold_right  (* forall stmt in block b *) 
              (fun stmt (live, stmts) ->   (* live:= { v | v in live variables of  b } stmts := {} *) 
               let live = Liveness.transfer stmt live in (* live := use[b] ∪ (live \ def[b]) *)
	       let stmts =
                   match stmt with
                     | { Block.inst = Block.CallDir(Id.L(l),_,_) } -> (* TODO : CallCls *)
                         let set = live in
                         let set = List.fold_left (fun acc (x, _) -> S.remove x acc) set stmt.Block.def  in
                         let (saves, restores) =
                           S.fold
                             (fun x (saves', restores') ->
			      let x' = Id.genid ("stack." ^ x) in
                              (Block.to_stmt  (Block.Save(x, x')) [] :: saves',
			       Block.to_stmt  (Block.Restore(x')) [x, (Asm.reg_type_of x)] :: restores'))
			     set ([], [])
                         in
                         saves @
                         [stmt] @
                         restores @
                         stmts
                     | _ -> stmt :: stmts
                 in
                 (live , stmts))  (* stmts := {newstmt} ∪ stmts *)
	      stmts (assert (M.mem b liveness.Liveness.out); M.find b liveness.Liveness.out, [])))
      cfg
  in
  { func with Block.blocks = cfg }

let calling_convention func args fargs ret =
  assert (List.for_all (fun x -> Asm.is_reg x) (ret :: args @ fargs));
  let cfg = func.Block.blocks in
  let Id.L(fname) = func.Block.name in
  let retvar = 
    match func.Block.ret with
    | Type.Unit -> []
    | _ -> [fname ^ "_ret"]
  in
  let env = 
    List.fold_left2 
      (fun acc arg reg ->
       if Asm.is_reg arg then assert (arg = reg);
       M.add arg reg acc) M.empty func.Block.args args
  in
  let env = 
    List.fold_left2 
      (fun acc farg freg ->
       if Asm.is_reg farg then assert (farg = freg);
       M.add farg freg acc) env func.Block.fargs fargs
  in
  let env = match retvar with
    | [] -> env
    | [x] -> M.add x ret env
    | _ -> assert false
  in

  let replace x = if M.mem x env then M.find x env else x in

  let cfg =
    Block.map_stmt_list
      (function
         | { Block.inst = Block.Entry; Block.def = def } as stmt ->
             { stmt with Block.def = List.map (fun (x, t) -> (replace x, t)) def } ::
             (Block.move_args func.Block.args args @  Block.move_fargs func.Block.fargs fargs)
	 | { Block.inst = Block.Ret; Block.use = use } as stmt ->
	    let init =
	      (match retvar with
	       | [] -> []
	       | [x] -> Block.move_args_with_type  [ret]  retvar func.Block.ret 
	       | _ -> assert false) 
	    in
	      init @
              [{ stmt with Block.use = List.map replace use }] 
         | stmt -> [stmt])
      cfg
  in
    { func with
      Block.blocks = cfg;
      Block.args = args;
      Block.fargs = fargs }

let assign_args args fargs =
  let (_, args') = 
    List.fold_right 
    (fun x (regs, acc) -> 
     match regs with
     | _ when Asm.is_reg x -> (regs, acc @ [x])
     | r :: rs -> (rs, acc @ [r])
     | _  -> Printf.fprintf stderr "too many arguments passed\n"; assert false)
    args (Asm.allregs, [])
  in
  let (_, fargs') = 
    List.fold_right 
    (fun x (fregs, acc) -> 
     match fregs with
     | _ when Asm.is_reg x -> (fregs, acc @ [x])
     | f :: fs -> (fs, acc @ [f])
     | _  -> Printf.fprintf stderr "too many arguments passed\n"; assert false)
    fargs (Asm.allfregs, [])
  in
  (args', fargs')


let par2arg prog =
  let prog =
    Block.map_func
      (fun l func ->
       let (args, fargs) = assign_args func.Block.args func.Block.fargs in
       let ret_type = func.Block.ret in
       let ret = Asm.ret_reg_of  ret_type in  
       calling_convention
         func
         args
	 fargs
         ret)
      prog
  in
  let prog =
    Block.map_cfg
      (fun l cfg ->
       Block.map_stmt_list
         (function (* TODO : CallCls *)
           | { Block.inst = Block.CallDir (Id.L(l), args, fargs); Block.use = use; Block.def = def } as stmt ->
	      let (retval, rettype) = 
		(match def with
		 | [x, t] -> ([x], t)
		 | [] -> ([], Type.Unit) 
		 | _ -> assert false)
	      in
	      (try 
		let Block.Prog(_, fundefs) = prog in (* add moves from arguments to paramator *)
		 let func = M.find l fundefs in
		 let init_stmt = 
		   (Block.move_args func.Block.args args) @ (Block.move_fargs func.Block.fargs fargs)
		 in
		 let fin_stmt = 
		   match rettype with
		   | Type.Unit -> []
		   | _ -> Block.move_args_with_type retval [(l ^ "_ret")] rettype 
		 in
		 let def = 
		   match rettype with
		   | Type.Unit -> []
		   | _ -> [(l ^ "_ret"), rettype]
		 in
		 init_stmt @
                   [{ stmt with
                      Block.inst = Block.CallDir(Id.L(l), func.Block.args, func.Block.fargs);
                      Block.use = func.Block.args @ func.Block.fargs;
                      Block.def = def }] @
		     fin_stmt
	      with Not_found -> (* external function *) (*move_argsの順番が怪しい*)
		   Printf.fprintf stderr "satisfying the calling convetion for ext-fun %s\n" l;
		   let n = List.length args in
		   let m = List.length fargs in
		   let rec take n xs =  (* TODO: 自作の標準ライブラリかリスト用のmoduleほしい. std.ml? l.ml? *)
		     (match (n, xs) with
		      | 0, _ -> []
		      | _, [] -> []
		      | n, x::xs -> x :: take (n - 1) xs) 
		   in
		   let actual_args = take n Asm.allregs in
		   let actual_fargs = take m Asm.allfregs in
		   let ret_reg = Asm.ret_reg_of rettype in
		   let init_stmt = 
		     (Block.move_args actual_args args) @ (Block.move_fargs actual_fargs fargs)
		   in
		   let fin_stmt = 
		     (match rettype with
		      | Type.Unit -> []
		      | _ -> Block.move_args_with_type retval [ret_reg] rettype) 
		   in
		   (*let fin_stmt = 
		   Block.move_args_with_type [retval] [ret_reg]  rettype in
		   Printf.fprintf stderr "%s\n" (Block.string_of_stmt (List.hd fin_stmt));*)
		   init_stmt @
                     [{ stmt with
			Block.inst = Block.CallDir(Id.L(l), actual_args, actual_fargs);
			Block.use = actual_args @ actual_fargs;
			Block.def = [ret_reg, rettype] }] @
		       fin_stmt)
	   | stmt -> [stmt])
         cfg)
      prog
  in
  prog
 

(* regAlloc for fdef *)
let g func =
  let Id.L(fname) = func.Block.name in
  Printf.fprintf stderr "register Allocation of %s\n" fname;
  (*Block.output_for_graphviz stderr func.Block.blocks;*)
  let func = GColoring.f func in
  let func = save_and_restore func in
  (*
  (* debug info *)
  let () =
    let used_regs = Block.fv func in
    let used_regs = S.union (S.of_list (func.Block.args @ func.Block.fargs)) used_regs in (* TODO : rets*)
    S.iter
      (fun x ->
         if not (Asm.is_reg x) then (
           Printf.fprintf stderr  "%s is not a register, but is not colored\n" x;
           Block.fold_stmt
             (fun stmt () ->
                List.iter (fun (y, t) -> if x = y then Printf.fprintf stderr  "%s is type of %s\n" x (Type.string_of_type t)) stmt.Block.def)
             func.Block.blocks ()
         )) used_regs;
    let unused = S.diff (S.of_list (Asm.allregs @ Asm.allfregs)) used_regs in
     Printf.fprintf stderr "unused registers: %d (regs: %d)\n" (S.cardinal unused) (S.cardinal (S.filter (fun x -> Asm.reg_type_of x <> Type.Float) unused)) 
  in *)
  func

(* main function *)    
let f prog =
  let prog = par2arg prog in
  let prog = Block.map_func (fun l func -> g func) prog in
    prog

