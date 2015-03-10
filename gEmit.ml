open Block

let stackset = ref S.empty 
let stackmap = ref [] 
let unused_stackpos = ref S.empty
let save x = 
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x = 
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad = 
       if List.length !stackmap mod 2 = 0 then [] else [Id.gentmp Type.Int] in
       stackmap := !stackmap @ pad @ [x; x])
let locate x = 
  let rec loc = function 
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
    loc !stackmap
let offset x = List.hd (locate x)
let stacksize () = Asm.align (List.length !stackmap + 1)
let load_label r label =
  "\tli\t" ^ (r) ^ ", " ^ label ^ "\n"


let emit_inst oc = function
  | { inst = Entry } -> ()
  | { inst = Nop } -> ()
  | { inst = Li(i); def = [x,_] }  when i >= -32768 && i < 32768 -> 
     Printf.fprintf oc "\tli\t%s, %d\n" x i 
  | { inst = Li(i); def = [x,_] } ->
      let n = i lsr 16 in
      let m = i lxor (n lsl 16) in
      let r =  x in
      Printf.fprintf oc "\tlis\t%s, %d\n" r n;
      Printf.fprintf oc "\taddil\t%s, %s, %d\n" r r m
  | { inst = FLi(Id.L(l)); def = [x,_] }-> Printf.fprintf oc "\tfload\t%s, $r00, %s\n" x l 
  | { inst = SetL(Id.L(y)); def = [x,_] } -> 
     let s = load_label x y in
     Printf.fprintf oc "%s" s
  | { inst = Mov(y); def = [x,_] } when x = y -> ()
  | { inst = Mov(y); def = [x,_] } -> Printf.fprintf oc "\tmov\t%s, %s\n" x y
  | { inst = Neg(y); def = [x,_] } -> Printf.fprintf oc "\tsub\t%s, $r00, %s\n" x y
  | { inst = Add(y, Asm.V(z)); def = [x,_] } -> Printf.fprintf oc "\tadd\t%s, %s, %s, 0\n" x y z
  | { inst = Add(y, Asm.C(z)); def = [x,_] } -> Printf.fprintf oc "\taddil\t%s, %s, %d\n" x y z 
  | { inst = Sub(y, Asm.V(z)); def = [x,_] } -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" x y z
  | { inst = Sub(y, Asm.C(z)); def = [x,_] } -> Printf.fprintf oc "\tsubi\t%s, %s, %d\n" x y z
  | { inst = Sll(y, Asm.V(z)); def = [x,_] } ->  Printf.fprintf oc "\tsll\t%s, %s, %s\n" x y z
  | { inst = Sll(y, Asm.C(z)); def = [x,_] } ->  Printf.fprintf oc "\tslli\t%s, %s, %d\n" x y z
  | { inst = Srl(y, Asm.V(z)); def = [x,_] } ->  Printf.fprintf oc "\tsrl\t%s, %s, %s\n" x y z
  | { inst = Srl(y, Asm.C(z)); def = [x,_] } ->  Printf.fprintf oc "\tsrli\t%s, %s, %d\n" x y z
  | { inst = Sla(y, Asm.V(z)); def = [x,_] } ->  Printf.fprintf oc "\tsla\t%s, %s, %s\n" x y z
  | { inst = Sla(y, Asm.C(z)); def = [x,_] } ->  Printf.fprintf oc "\tslai\t%s, %s, %d\n" x y z
  | { inst = Sra(y, Asm.V(z)); def = [x,_] } ->  Printf.fprintf oc "\tsra\t%s, %s, %s\n" x y z
  | { inst = Sra(y, Asm.C(z)); def = [x,_] } ->  Printf.fprintf oc "\tsrai\t%s, %s, %d\n" x y z
  | { inst = Load(y, Asm.V(z)); def = [x,_] } -> Printf.fprintf oc "\tloadr\t%s, %s, %s\n" x y z
  | { inst = Load(y, Asm.C(z)); def = [x,_] } -> Printf.fprintf oc "\tload\t%s, %s, %d\n" x y z 
  | { inst = Loadi(y); def = [x,_] } -> Printf.fprintf oc "\tloadi\t%s, %d\n" x y
  | { inst = Store(x, y, Asm.V(z)) } -> Printf.fprintf oc "\tstorer\t%s, %s, %s\n" x y z
  | { inst = Store(x, y, Asm.C(z)) } -> Printf.fprintf oc "\tstore\t%s, %s, %d\n" x y z
  | { inst = Storei(x, y) } -> Printf.fprintf oc "\tstorei\t%s, %d\n" x y
  | { inst = FMov(y); def = [x,_] } when x = y -> ()
  | { inst = FMov(y); def = [x,_] } -> Printf.fprintf oc "\tfadd\t%s, $f00,  %s\n" x y (*TODO*)
  | { inst = FNeg(y); def = [x,_] } -> Printf.fprintf oc "\tfsub\t%s, $f00,  %s\n" x y (*TODO*)
  | { inst = FAdd(y, z); def = [x,_] } -> Printf.fprintf oc "\tfadd\t%s, %s, %s\n" x y z
  | { inst = FSub(y, z); def = [x,_] } -> Printf.fprintf oc "\tfsub\t%s, %s, %s\n" x y z
  | { inst = FMul(y, z); def = [x,_] } -> Printf.fprintf oc "\tfmul\t%s, %s, %s\n" x y z
  | { inst = FDiv(y, z); def = [x,_] } -> Printf.fprintf oc "\tfdiv\t%s, %s, %s\n" x y z
  | { inst = Itof(y); def = [x,_] } -> Printf.fprintf oc "\titof\t%s, %s\n" x y
  | { inst = Ftoi(y); def = [x,_] } -> Printf.fprintf oc "\tftoi\t%s, %s\n" x y
  | { inst = FLoad(y, Asm.V(z)); def = [x, _] } -> Printf.fprintf oc "\tfloadr\t%s, %s, %s\n" x y z
  | { inst = FLoad(y, Asm.C(z)); def = [x, _] } -> Printf.fprintf oc "\tfload\t%s, %s, %d\n" x y z
  | { inst = FLoadi(y); def = [x, _] } -> Printf.fprintf oc "\tfloadi\t%s, %d\n" x y
  | { inst = FStore(x, y, Asm.V(z)) } -> Printf.fprintf oc "\tfstorer\t%s, %s, %s\n" x y z
  | { inst = FStore(x, y, Asm.C(z)) } -> Printf.fprintf oc "\tfstore\t%s, %s, %d\n" x y z
  | { inst = FStorei(x, y) } -> Printf.fprintf oc "\tfstorei\t%s, %d\n" x y 
  | { inst = Comment(s) } -> Printf.fprintf oc "#\t%s\n" s
  | { inst = Write(x) } -> Printf.fprintf oc "\twrite\t%s\n" x
  | { inst = Xor(y, z); def = [x, _] } -> Printf.fprintf oc "\txor\t%s, %s, %s\n" x y z
  | { inst = FInv(y); def = [x, _] } -> Printf.fprintf oc "\tfinv\t%s, %s\n" x y
  | { inst = FSqrt(y); def = [x, _] } -> Printf.fprintf oc "\tfsqrt\t%s, %s\n" x y
  (* function call *)
  | { inst = CallCls _ }-> assert false
  | { inst = CallDir(Id.L(l),_,_) } ->
     let ss = stacksize () in
     Printf.fprintf oc "\taddil\t%s, %s, %d\n" Asm.reg_sp Asm.reg_sp (ss + 1);
	Printf.fprintf oc "\tcall\t%s\n" l;
	Printf.fprintf oc "\tsubi\t%s, %s, %d\n" Asm.reg_sp Asm.reg_sp (ss + 1);
  (* save and restore *)
  | { inst = Save("$r00", y) } | { inst = Save("$r13", y) }   |{ inst = Save("$f00", y) } -> 
     (*unused_stackpos := S.add y !unused_stackpos;*)
    ()
  | { inst = Save(x, y) }
       when List.mem x Asm.allregs && not (S.mem y !stackset) ->
     save y;
     Printf.fprintf oc "\tstore\t%s, %s, %d\n" x Asm.reg_sp (offset y) 
  | { inst = Save(x, y) }
       when List.mem x Asm.allfregs && not (S.mem y !stackset) ->
     savef y;
    Printf.fprintf oc "\tfstore\t%s, %s, %d\n" x Asm.reg_sp (offset y) 
  | { inst = Save(x, y) } -> 
     assert (if S.mem y !stackset then true 
	     else (Printf.fprintf stderr "%s %s\n" x y; false));
     ()
  | { inst = Restore(y); def = ["$r00",_] } | { inst = Restore(y); def = ["$r13",_] } |
  { inst = Restore(y); def = ["$f00",_] } -> ()
  | { inst = Restore(y); def = [x, _] }
       when List.mem x Asm.allregs ->
     Printf.fprintf oc "\tload\t%s, %s, %d\n" x Asm.reg_sp (offset y) 
  | { inst = Restore(y); def = [x, _] } ->
     assert (List.mem x Asm.allfregs);
     Printf.fprintf oc "\tfload\t%s, %s, %d\n" x Asm.reg_sp (offset y) 
  | stmt -> 
     Printf.fprintf stderr "%s\n" (string_of_stmt stmt);
     assert false


let emit_if oc v = function
  | { inst = IfEq(x, y) } -> Printf.fprintf oc "\tbeqi\t%s, %s, %s\n" x y v
  | { inst = IfLT(x, y) } -> Printf.fprintf oc "\tblti\t%s, %s, %s\n" x y v
  | { inst = IfFEq(x, y) } -> Printf.fprintf oc "\tbfeqi\t%s, %s, %s\n" x y v
  | { inst = IfFLT(x, y) } -> Printf.fprintf oc "\tbflti\t%s, %s, %s\n" x y v
  | _ -> assert false


let emit_block oc stmts = 
  let (branch, stmts) =
    match List.rev stmts with
      | branch :: stmts' -> (branch, List.rev stmts')
      | _ -> assert false 
  in
    List.iter (emit_inst oc) stmts;
    branch

(* emit each function *)
let g oc fname fdef  = 
  let { blocks = cfg; head = head } = fdef in
  stackset := S.empty;
  stackmap := [];
  let rec dfs vs v = 
    if S.mem v vs then vs
    else
      let vs = S.add v vs in
        Printf.fprintf oc "%s:\n" v;
        let branch = emit_block oc (CFG.find v cfg) in
        let edges = CFG.expand (fun w e_tag acc -> (e_tag, w) :: acc) v cfg [] in 
          match branch with
            | { inst = (IfEq _ | IfLT _ | IfFEq _ | IfFLT _) } ->
                assert (List.mem_assoc (Some Then) edges && List.mem_assoc (Some Else) edges);
                let b_then = List.assoc (Some Then) edges in
                let b_else = List.assoc (Some Else) edges in
                  emit_if oc b_then branch;
                  let vs =
                    if S.mem b_else vs then 
                      (Printf.fprintf oc "\tbeqi\t $r00, $r00, %s\n" b_else; vs)
                    else (* remove redundunt jump *)
                      dfs vs b_else
                  in
                    dfs vs b_then 
	    | { inst = Ret } when fname = "__main__"-> 
	       Printf.fprintf oc "   # main program end\n";
	       Printf.fprintf oc "\tbeq\t$r00, $r00, $r15, 0\n";
	       vs
            | { inst = Ret } ->
                Printf.fprintf oc "\tret\n";
                vs
            | { inst = Jmp } ->
                assert (List.mem_assoc None edges);
                let w = List.assoc None edges in
                  if S.mem w vs then 
                    (Printf.fprintf oc "\tbeqi\t $r00, $r00, %s\n" w; vs)
                  else (* remove redundunt jump *)
                    dfs vs w
            | { inst = Goto(Id.L(l)) } when fname = "__main__" -> (* cannot use goto in main*)
	       Printf.fprintf oc "\tcall\t%s\n" l;
	       Printf.fprintf oc "   # main program end\n";
	       Printf.fprintf oc "\tbeq\t$r00, $r00, $r15, 0\n";
	       vs
            | { inst = Goto(Id.L(l)) } -> 
	       Printf.fprintf oc "\tbeqi\t$r00, $r00, %s\n" l;
	       vs  
            | stmt -> 
	       Printf.fprintf stderr "%s\n" (string_of_stmt stmt);
	       assert false
  in
  if fname = "__main__" then 
    (Printf.fprintf oc "main: # main entry point\n";
     Printf.fprintf oc "   # main program start\n")
  else Printf.fprintf oc "%s:\n" fname ;
  ignore (dfs S.empty head)

let f oc (Prog(data, fundefs)) =
  Printf.fprintf stderr "generating assembly...\n";
  (if data <> [] then
     (Printf.fprintf oc "\t.data\n";
      List.iter
	(fun (Id.L(x), d) ->
	 Printf.fprintf oc "%s:\n" x;
	 Printf.fprintf oc "\t.float\t%f\n" d)
	data));
  Printf.fprintf oc "\t.text\n";
  Printf.fprintf oc "\t.globl  main\n";
  M.iter (fun fname fdef -> if fname = "__main__" then () else g oc fname fdef) fundefs;
  let main = M.find "__main__" fundefs in
  g oc "__main__" main 
