open Asm

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

let stackset = ref S.empty (* すでに Save された変数の集合 *)
let stackmap = ref [] (* Save された変数のスタックにおける位置 *)
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
let stacksize () = align (List.length !stackmap + 1)

let reg r = (*$を消す作業*)
  if is_reg r 
  then String.sub r 1 (String.length r - 1)
  else r 

let load_label r label =
  "\tli\t" ^ (r) ^ ", " ^ label ^ "\n"
  (*"\tlis\t" ^ ( r) ^ ", ha16(" ^ label ^ ")\n" ^
    "\taddi\t" ^ ( r) ^ ", " ^ ( r) ^ ", lo16(" ^ label ^ ")\n"*)

(* 関数呼び出しのために引数を並べ替える (register shuffling) *)
let rec shuffle sw xys = 
  (* remove identical moves *)
  let (_, xys) = List.partition (fun (x, y) -> x = y) xys in
    (* find acyclic moves *)
    match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
      | ([], []) -> []
      | ((x, y) :: xys, []) -> (* no acyclic moves; resolve a cyclic move *)
	  (y, sw) :: (x, y) :: 
	    shuffle sw (List.map (function 
				    | (y', z) when y = y' -> (sw, z)
				    | yz -> yz) xys)
      | (xys, acyc) -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t (* 末尾かどうかを表すデータ型 *)
let rec g oc = function (* 命令列のアセンブリ生成 *)
  | (dest, Ans (exp)) -> g' oc (dest, exp)
  | (dest, Let((x, t), exp, e)) -> g' oc (NonTail (x), exp); g oc (dest, e)
and g' oc = function (* 各命令のアセンブリ生成 *)
    (* 末尾でなかったら計算結果を dest にセット *)
  | (NonTail(_), Nop) -> ()
  | (NonTail(x), Li(i)) when i >= -32768 && i < 32768 -> 
      Printf.fprintf oc "\tli\t%s, %d\n" x i
  | (NonTail(x), Li(i)) ->
      let n = i lsr 16 in
      let m = i lxor (n lsl 16) in
      let r =  x in
	Printf.fprintf oc "\tlis\t%s, %d\n" r n;
	Printf.fprintf oc "\taddil\t%s, %s, %d\n" r r m
  | (NonTail(x), FLi(Id.L(l))) ->
      (*let s = load_label reg_tmp l in
      Printf.fprintf oc "%s\tfload\t%s, %s, 0\n" s x reg_tmp*)
     Printf.fprintf oc "\tfload\t%s, $r00, %s\n" x l 
  | (NonTail(x), SetL(Id.L(y))) -> 
      let s = load_label x y in
      Printf.fprintf oc "%s" s
  | (NonTail(x), Mov(y)) when x = y -> ()
  | (NonTail(x), Mov(y)) -> Printf.fprintf oc "\tmov\t%s, %s\n" x y
  | (NonTail(x), Neg(y)) -> Printf.fprintf oc "\tsub\t%s, $r00, %s\n" x y
  | (NonTail(x), Add(y, V(z))) -> 
      Printf.fprintf oc "\tadd\t%s, %s, %s, 0\n" x y z
 | (NonTail(x), Add(y, C(z))) -> 
      Printf.fprintf oc "\taddil\t%s, %s, %d\n" x y z 
  | (NonTail(x), Sub(y, V(z))) -> 
      Printf.fprintf oc "\tsub\t%s, %s, %s\n" x y z
 | (NonTail(x), Sub(y, C(z))) -> 
      Printf.fprintf oc "\tsubi\t%s, %s, %d\n" x y z
  | (NonTail(x), Sll(y, V(z))) -> 
      Printf.fprintf oc "\tsll\t%s, %s, %s\n" x y z
  | (NonTail(x), Sll(y, C(z))) -> 
      Printf.fprintf oc "\tslli\t%s, %s, %d\n" x y z
  | (NonTail(x), Sla(y, V(z))) -> 
      Printf.fprintf oc "\tsla\t%s, %s, %s\n" x y z
  | (NonTail(x), Sla(y, C(z))) -> 
      Printf.fprintf oc "\tslai\t%s, %s, %d\n" x y z
  | (NonTail(x), Srl(y, V(z))) -> 
      Printf.fprintf oc "\tsrl\t%s, %s, %s\n" x y z
  | (NonTail(x), Srl(y, C(z))) -> 
      Printf.fprintf oc "\tsrli\t%s, %s, %d\n" x y z
  | (NonTail(x), Sra(y, V(z))) -> 
      Printf.fprintf oc "\tsra\t%s, %s, %s\n" x y z
  | (NonTail(x), Sra(y, C(z))) -> 
      Printf.fprintf oc "\tsrai\t%s, %s, %d\n" x y z
  | (NonTail(x), Load(y, V(z))) -> 
     Printf.fprintf oc "\tloadr\t%s, %s, %s\n" x y z
     (*Printf.fprintf oc "\tadd\t$r11, %s, %s, 0\n\tload\t%s, $r11, 0\n" y z x*)
  | (NonTail(x), Load(y, C(z))) -> 
      Printf.fprintf oc "\tload\t%s, %s, %d\n" x y z 
  | (NonTail(x), Loadi(y)) -> 
      Printf.fprintf oc "\tloadi\t%s, %d\n" x y
  | (NonTail(_), Store(x, y, V(z))) ->
     Printf.fprintf oc "\tstorer\t%s, %s, %s\n" x y z
     (*Printf.fprintf oc "\tadd\t$r11, %s, %s, 0\n\tstore\t%s, $r11, 0\n" y z x*)
  | (NonTail(_), Store(x, y, C(z))) -> 
      Printf.fprintf oc "\tstore\t%s, %s, %d\n" x y z
  | (NonTail(_), Storei(x, y)) -> 
      Printf.fprintf oc "\tstorei\t%s, %d\n" x y
  | (NonTail(x), FMov(y)) when x = y -> ()
  | (NonTail(x), FMov(y)) -> Printf.fprintf oc "\tfadd\t%s, $f00,  %s\n" x y
  | (NonTail(x), FNeg(y)) -> 
      Printf.fprintf oc "\tfsub\t%s, $f00,  %s\n" x y
  | (NonTail(x), FAdd(y, z)) -> 
      Printf.fprintf oc "\tfadd\t%s, %s, %s\n" x y z
  | (NonTail(x), FSub(y, z)) -> 
      Printf.fprintf oc "\tfsub\t%s, %s, %s\n" x y z
  | (NonTail(x), FMul(y, z)) -> 
      Printf.fprintf oc "\tfmul\t%s, %s, %s\n" x y z
  | (NonTail(x), FDiv(y, z)) -> 
      Printf.fprintf oc "\tfdiv\t%s, %s, %s\n" x y z
  | (NonTail(x), Itof(y)) -> 
      Printf.fprintf oc "\titof\t%s, %s\n" x y
  | (NonTail(x), Ftoi(y)) -> 
      Printf.fprintf oc "\tftoi\t%s, %s\n" x y
  | (NonTail(x), FLoad(y, V(z))) ->
      Printf.fprintf oc "\tfloadr\t%s, %s, %s\n" x y z
      (*Printf.fprintf oc "\tadd\t$r11, %s, %s, 0\n\tfload\t%s, $r11, 0\n" y z x*)
  | (NonTail(x), FLoad(y, C(z))) -> 
      Printf.fprintf oc "\tfload\t%s, %s, %d\n" x y z
  | (NonTail(x), FLoadi(y)) -> 
      Printf.fprintf oc "\tfloadi\t%s, %d\n" x y
  | (NonTail(_), FStore(x, y, V(z))) ->
      Printf.fprintf oc "\tfstorer\t%s, %s, %s\n" x y z
      (*Printf.fprintf oc "\tadd\t$r11, %s, %s, 0\n\tfstore\t%s, $r11, 0\n" y z x*)
  | (NonTail(_), FStore(x, y, C(z))) ->
      Printf.fprintf oc "\tfstore\t%s, %s, %d\n" x y z
  | (NonTail(_), FStorei(x, y)) ->
      Printf.fprintf oc "\tfstorei\t%s, %d\n" x y 
  | (NonTail(_), Comment(s)) -> Printf.fprintf oc "#\t%s\n" s
  | (NonTail(_), Write(x)) -> Printf.fprintf oc "\twrite\t%s\n" x
  | (NonTail(x), Xor(y, z)) -> Printf.fprintf oc "\txor\t%s, %s, %s\n" x y z
  | (NonTail(x), FInv(y)) -> Printf.fprintf oc "\tfinv\t%s, %s\n" x y
  | (NonTail(x), FSqrt(y)) -> Printf.fprintf oc "\tfsqrt\t%s, %s\n" x y
  (* 退避の仮想命令の実装 *)
  | (NonTail(_), Save(x, y))
      when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
	Printf.fprintf oc "\tstore\t%s, %s, %d\n" x reg_sp (offset y) 
  | (NonTail(_), Save(x, y)) 
      when List.mem x allfregs && not (S.mem y !stackset) ->
      savef y;
	Printf.fprintf oc "\tfstore\t%s, %s, %d\n" x reg_sp (offset y) 
  | (NonTail(_), Save(x, y)) -> assert (S.mem y !stackset); ()
  (* 復帰の仮想命令の実装 *)
  | (NonTail(x), Restore(y)) when List.mem x allregs ->
      Printf.fprintf oc "\tload\t%s, %s, %d\n" x reg_sp (offset y) 
  | (NonTail(x), Restore(y)) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tfload\t%s, %s, %d\n" x reg_sp (offset y)   (* 末尾だったら計算結果を第一レジスタにセット *)
  | (Tail, (Nop | Store _ | Storei _ |  FStore _ | FStorei _ |  Comment _ | Save _ | Write _ as exp)) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tret\n";
  | (Tail, (Li _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Sll _ | Sla _ | Srl _ | Sra _ | 
            Load _ | Loadi _ | Ftoi _ | Xor _ as exp)) -> 
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "\tret\n";
  | (Tail, (FLi _ | FMov _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ |
            FLoad _ | FLoadi _ | Itof _ |FInv _ | FSqrt _ as exp)) ->
      g' oc (NonTail(fregs.(0)), exp);
      Printf.fprintf oc "\tret\n";
  | (Tail, (Restore(x) as exp)) ->
      (match locate x with
	 | [i] -> g' oc (NonTail(regs.(0)), exp)
	 | [i; j] when (i + 1 = j) -> g' oc (NonTail(fregs.(0)), exp)
	 | _ -> assert false);
      Printf.fprintf oc "\tret\n";
  | (Tail, IfEq(x, y, e1, e2)) ->
      g'_tail_if oc e1 e2 "beq" "bne" x y
  | (Tail, IfLT(x, y, e1, e2)) ->
      g'_tail_if oc e1 e2 "blt" "bge" x y
  | (Tail, IfFEq(x, y, e1, e2)) ->
      g'_tail_if oc e1 e2 "bfeq" "bfne" x y
  | (Tail, IfFLT(x, y, e1, e2)) ->
      g'_tail_if oc e1 e2 "bflt" "bfge" x y
  | (NonTail(z), IfEq(x, y, e1, e2)) ->
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" "bne" x y
  | (NonTail(z), IfLT(x, y, e1, e2)) ->
      g'_non_tail_if oc (NonTail(z)) e1 e2 "blt" "bge" x y
  | (NonTail(z), IfFEq(x, y, e1, e2)) ->
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bfeq" "bfne" x y
  | (NonTail(z), IfFLT(x, y, e1, e2)) ->
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bflt" "bfge" x y
  (* 関数呼び出しの仮想命令の実装 *)
  | (Tail, CallCls(x, ys, zs)) -> (* 末尾呼び出し *)
      g'_args oc [(x, reg_cl)] ys zs;
      Printf.fprintf oc "\tload\t%s, %s, 0\n" (reg_sw) (reg_cl);
      Printf.fprintf oc "\tmov\t%s, %s\n\tbeq\t$r00, $r00, %s, 0\n" cnt_reg (reg_sw) cnt_reg; 
  | (Tail, CallDir(Id.L(x), ys, zs)) -> (* 末尾呼び出し *)
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tbeqi\t $r00, $r00, %s\n" x
  | (NonTail(a), CallCls(x, ys, zs)) ->
      (*Printf.fprintf oc "\tload\t%s, %s, -4\n" reg_tmp reg_sp;*) 
      g'_args oc [(x, reg_cl)] ys zs;
      let ss = stacksize () in
	(*Printf.fprintf oc "\tstore\t%s, %s, %d\n" reg_tmp reg_sp (ss - 4);*)
	Printf.fprintf oc "\taddil\t%s, %s, %d\n" reg_sp reg_sp (ss + 1);
	Printf.fprintf oc "\tload\t%s, %s, 0\n" cnt_reg (reg_cl);
	Printf.fprintf oc "\taddil\t%s, %s, 3\n\tstore\t%s, %s, 0\n\tbeq\t$r00, $r00, %s\n" reg_tmp pc reg_tmp reg_sp cnt_reg; (*callと同等*)
	Printf.fprintf oc "\tsubi\t%s, %s, %d\n" reg_sp reg_sp (ss + 1);
	(*Printf.fprintf oc "\tload\t%s, %s, %d\n" reg_tmp reg_sp (ss - 4);*)
	(if List.mem a allregs && a <> regs.(0) then 
	   Printf.fprintf oc "\tmov\t%s, %s\n" (a) (regs.(0)) 
	 else if List.mem a allfregs && a <> fregs.(0) then 
	   Printf.fprintf oc "\tfadd\t%s, $f00, %s\n" (a) (fregs.(0)));
	(*Printf.fprintf oc "\tstore\t%s, %s, -4\n"  reg_tmp reg_sp*) 
  | (NonTail(a), CallDir(Id.L(x), ys, zs)) -> 
      (*Printf.fprintf oc "\tload\t%s, %s, -4\n" reg_tmp reg_sp;*) 
      g'_args oc [] ys zs;
      let ss = stacksize () in
	(*Printf.fprintf oc "\tstore\t%s, %s, %d\n" reg_tmp reg_sp (ss - 4);*)
	Printf.fprintf oc "\taddil\t%s, %s, %d\n" reg_sp reg_sp (ss + 1);
	Printf.fprintf oc "\tcall\t%s\n" x;
	Printf.fprintf oc "\tsubi\t%s, %s, %d\n" reg_sp reg_sp (ss + 1);
	(*Printf.fprintf oc "\tload\t%s, %s, %d\n" reg_tmp reg_sp (ss - 4);*)
	(if List.mem a allregs && a <> regs.(0) then
	   Printf.fprintf oc "\tmov\t%s, %s\n" (a) (regs.(0))
	 else if List.mem a allfregs && a <> fregs.(0) then
	   Printf.fprintf oc "\tfadd\t%s, $f00, %s\n" (a) (fregs.(0)));
	(*Printf.fprintf oc "\tstore\t%s, %s, -4\n" reg_tmp reg_sp*)
and g'_tail_if oc e1 e2 b bn x y = 
  let b_else = Id.genid (bn ^ "_else") in
    Printf.fprintf oc "\t%si\t%s, %s, %s\n" b x y b_else;
    let stackset_back = !stackset in
      g oc (Tail, e2);
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (Tail, e1)
and g'_non_tail_if oc dest e1 e2 b bn x y = 
  let b_else = Id.genid (bn ^ "_else") in
  let b_cont = Id.genid (bn ^ "_cont") in
    Printf.fprintf oc "\t%si\t%s, %s, %s\n" b x y b_else;
    let stackset_back = !stackset in
      g oc (dest, e2);
      let stackset1 = !stackset in
      Printf.fprintf oc "\tbeqi\t$r00, $r00, %s\n" b_cont;
	Printf.fprintf oc "%s:\n" b_else;
	stackset := stackset_back;
	g oc (dest, e1);
	Printf.fprintf oc "%s:\n" b_cont;
	let stackset2 = !stackset in
	  stackset := S.inter stackset1 stackset2

and g'_args oc x_reg_cl ys zs = 
  let (i, yrs) = 
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl) ys in
    List.iter
      (fun (y, r) -> Printf.fprintf oc "\tmov\t%s, %s\n" r y)
      (shuffle reg_sw yrs);
    let (d, zfrs) = 
      List.fold_left
	(fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
	(0, []) zs in
      List.iter
        (fun (z, fr) -> Printf.fprintf oc "\tfadd\t%s, $f00, %s\n" fr z)
	(shuffle reg_fsw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) = 
  Format.eprintf "generating assembly...@.";
  (if data <> [] then
    (Printf.fprintf oc "\t.data\n";
     List.iter
       (fun (Id.L(x), d) ->
	 Printf.fprintf oc "%s:\n" x;
	 Printf.fprintf oc "\t.float\t%f\n" d)
       data));
  Printf.fprintf oc "\t.text\n";
  Printf.fprintf oc "\t.globl  main\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc "main: # main entry point\n";
  Printf.fprintf oc "   # main program start\n";
  stackset := S.empty;
  stackmap := [];
  g oc (NonTail("$r01"), e);
  Printf.fprintf oc "   # main program end\n";
  Printf.fprintf oc "\tbeq\t$r00, $r00, $r15, 0\n"
