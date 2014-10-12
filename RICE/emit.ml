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
let offset x = 4 * List.hd (locate x)
let stacksize () = align ((List.length !stackmap + 1) * 4)

let reg r = (*%を消す作業*)
  if is_reg r 
  then String.sub r 1 (String.length r - 1)
  else r 

let load_label r label =
  "\tlis\t" ^ (reg r) ^ ", ha16(" ^ label ^ ")\n" ^
  "\taddi\t" ^ (reg r) ^ ", " ^ (reg r) ^ ", lo16(" ^ label ^ ")\n"

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
      Printf.fprintf oc "\tli\t%s, %d\n" (reg x) i
  | (NonTail(x), Li(i)) ->
      let n = i lsr 16 in
      let m = i lxor (n lsl 16) in
      let r = reg x in
	Printf.fprintf oc "\tlis\t%s, %d\n" r n;
	Printf.fprintf oc "\tori\t%s, %s, %d\n" r r m
  | (NonTail(x), FLi(Id.L(l))) ->
      let s = load_label reg_tmp l in
      Printf.fprintf oc "%s\tlfd\t%s, 0(%s)\n" s (reg x) reg_tmp
  | (NonTail(x), SetL(Id.L(y))) -> 
      let s = load_label x y in
      Printf.fprintf oc "%s" s
  | (NonTail(x), Mov(y)) when x = y -> ()
  | (NonTail(x), Mov(y)) -> Printf.fprintf oc "\tmov\t%s, %s\n" (reg x) (reg y)
  | (NonTail(x), Neg(y)) -> Printf.fprintf oc "\tneg\t%s, %s\n" (reg x) (reg y)
  | (NonTail(x), Add(y, z, C(i))) -> 
      Printf.fprintf oc "\tadd\t%s, %s, %s, %d\n" (reg x) (reg y) (reg z) i
 (* | (NonTail(x), Add(y, C(z))) -> 
      Printf.fprintf oc "\taddi\t%s, %s, %d\n" (reg x) (reg y) z *)
  | (NonTail(x), Sub(y, z)) -> 
      Printf.fprintf oc "\tsub\t%s, %s, %s\n" (reg x) (reg y) (reg z)
 (* | (NonTail(x), Sub(y, C(z))) -> 
      Printf.fprintf oc "\tsubi\t%s, %s, %d\n" (reg x) (reg y) z*)
  | (NonTail(x), Sll(y, V(z))) -> 
      Printf.fprintf oc "\tsll\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), Sll(y, C(z))) -> 
      Printf.fprintf oc "\tslli\t%s, %s, %d\n" (reg x) (reg y) z
  | (NonTail(x), Sla(y, V(z))) -> 
      Printf.fprintf oc "\tsla\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), Sla(y, C(z))) -> 
      Printf.fprintf oc "\tslai\t%s, %s, %d\n" (reg x) (reg y) z
  | (NonTail(x), Srl(y, V(z))) -> 
      Printf.fprintf oc "\tsrl\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), Srl(y, C(z))) -> 
      Printf.fprintf oc "\tsrli\t%s, %s, %d\n" (reg x) (reg y) z
  | (NonTail(x), Sra(y, V(z))) -> 
      Printf.fprintf oc "\tsra\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), Sra(y, C(z))) -> 
      Printf.fprintf oc "\tsrai\t%s, %s, %d\n" (reg x) (reg y) z
  | (NonTail(x), Load(y, _, V(z))) ->
      Printf.fprintf oc "\tloadr\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), Load(y, _, C(z))) -> 
      Printf.fprintf oc "\tload\t%s, %s, %d\n" (reg x) (reg y) z
  | (NonTail(_), Store(x, y, V(z))) ->
      Printf.fprintf oc "\tstorer\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(_), Store(x, y, C(z))) -> 
      Printf.fprintf oc "\tstore\t%s, %s, %d\n" (reg x) (reg y) z
  | (NonTail(x), FMov(y)) when x = y -> ()
  | (NonTail(x), FMov(y)) -> Printf.fprintf oc "\tfmov\t%s, %s\n" (reg x) (reg y)
  | (NonTail(x), FNeg(y)) -> 
      Printf.fprintf oc "\tfneg\t%s, %s\n" (reg x) (reg y)
  | (NonTail(x), FAdd(y, z)) -> 
      Printf.fprintf oc "\tfadd\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), FSub(y, z)) -> 
      Printf.fprintf oc "\tfsub\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), FMul(y, z)) -> 
      Printf.fprintf oc "\tfmul\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), FDiv(y, z)) -> 
      Printf.fprintf oc "\tfdiv\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), FLoad(y, _,  V(z))) ->
      Printf.fprintf oc "\tfloadr\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(x), FLoad(y, _, C(z))) -> 
      Printf.fprintf oc "\tfload\t%s, %s, %d\n" (reg x) (reg y) z
  | (NonTail(_), FStore(x, y, V(z))) ->
      Printf.fprintf oc "\tfstorer\t%s, %s, %s\n" (reg x) (reg y) (reg z)
  | (NonTail(_), FStore(x, y, C(z))) ->
      Printf.fprintf oc "\tfstore\t%s, %d(%s)\n" (reg x) z (reg y)
  | (NonTail(_), Comment(s)) -> Printf.fprintf oc "#\t%s\n" s
  (* 退避の仮想命令の実装 *)
  | (NonTail(_), Save(x, y))
      when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
	Printf.fprintf oc "\tstore\t%s, %s, %d\n" (reg x) reg_sp (offset y) 
  | (NonTail(_), Save(x, y)) 
      when List.mem x allfregs && not (S.mem y !stackset) ->
      savef y;
	Printf.fprintf oc "\tfstore\t%s, %s, %d\n" (reg x) reg_sp (offset y) 
  | (NonTail(_), Save(x, y)) -> assert (S.mem y !stackset); ()
  (* 復帰の仮想命令の実装 *)
  | (NonTail(x), Restore(y)) when List.mem x allregs ->
      Printf.fprintf oc "\tload\t%s, %s, %d\n" (reg x) reg_sp (offset y) 
  | (NonTail(x), Restore(y)) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tfload\t%s, %s, %d\n" (reg x) reg_sp (offset y)   (* 末尾だったら計算結果を第一レジスタにセット *)
  | (Tail, (Nop | Store _ | FStore _ | Comment _ | Save _ as exp)) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tret\n";
  | (Tail, (Li _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Sll _ | Sla _ | Srl _ | Sra _ |
            Load _ as exp)) -> 
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "\tret\n";
  | (Tail, (FLi _ | FMov _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ |
            FLoad _ as exp)) ->
      g' oc (NonTail(fregs.(0)), exp);
      Printf.fprintf oc "\tret\n";
  | (Tail, (Restore(x) as exp)) ->
      (match locate x with
	 | [i] -> g' oc (NonTail(regs.(0)), exp)
	 | [i; j] when (i + 1 = j) -> g' oc (NonTail(fregs.(0)), exp)
	 | _ -> assert false);
      Printf.fprintf oc "\tret\n";
  | (Tail, IfEq(x, V(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);*)
      g'_tail_if oc e1 e2 "beq" "bne" x y
 | (Tail, IfEq(x, C(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;*)
      g'_tail_if_imm oc e1 e2 "beq" "bne" x y
  | (Tail, IfLE(x, V(y), e1, e2)) ->
     (* Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);*)
      g'_tail_if oc e1 e2 "ble" "bgt" x y
  | (Tail, IfLE(x, C(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;*)
      g'_tail_if_imm oc e1 e2 "ble" "bgt" x y
  | (Tail, IfGE(x, V(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);*)
      g'_tail_if oc e1 e2 "bge" "blt" x y
  | (Tail, IfGE(x, C(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;*)
      g'_tail_if_imm oc e1 e2 "bge" "blt" x y
  | (Tail, IfFEq(x, y, e1, e2)) ->
      (*Printf.fprintf oc "\tfcmpu\tcr7, %s, %s\n" (reg x) (reg y);*)
      g'_tail_if oc e1 e2 "beq" "bne" x y
  | (Tail, IfFLE(x, y, e1, e2)) ->
      (*Printf.fprintf oc "\tfcmpu\tcr7, %s, %s\n" (reg x) (reg y);*)
      g'_tail_if oc e1 e2 "ble" "bgt" x y
  | (NonTail(z), IfEq(x, V(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);*)
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" "bne" x y
  | (NonTail(z), IfEq(x, C(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;*)
      g'_non_tail_if_imm oc (NonTail(z)) e1 e2 "beq" "bne" x y
  | (NonTail(z), IfLE(x, V(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);*)
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bgt" x y
  | (NonTail(z), IfLE(x, C(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;*)
      g'_non_tail_if_imm oc (NonTail(z)) e1 e2 "ble" "bgt" x y
  | (NonTail(z), IfGE(x, V(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);*)
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bge" "blt" x y
  | (NonTail(z), IfGE(x, C(y), e1, e2)) ->
      (*Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;*)
      g'_non_tail_if_imm oc (NonTail(z)) e1 e2 "bge" "blt" x y  
  | (NonTail(z), IfFEq(x, y, e1, e2)) ->
      (*Printf.fprintf oc "\tfcmpu\tcr7, %s, %s\n" (reg x) (reg y);*)
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" "bne" x y
  | (NonTail(z), IfFLE(x, y, e1, e2)) ->
      (*Printf.fprintf oc "\tfcmpu\tcr7, %s, %s\n" (reg x) (reg y);*)
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bgt" x y
  (* 関数呼び出しの仮想命令の実装 *)
  | (Tail, CallCls(x, ys, zs)) -> (* 末尾呼び出し *)
      g'_args oc [(x, reg_cl)] ys zs;
      Printf.fprintf oc "\tload\t%s, %s, 0\n" (reg reg_sw) (reg reg_cl);
      Printf.fprintf oc "\tmtctr\t%s\n\tbctr\n" (reg reg_sw); (*TODO: PCの設定*)
  | (Tail, CallDir(Id.L(x), ys, zs)) -> (* 末尾呼び出し *)
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tb\t%s\n" x
  | (NonTail(a), CallCls(x, ys, zs)) ->
      Printf.fprintf oc "\tmflr\t%s\n" reg_tmp; (*TODO: リンクレジスタの設定*)
      g'_args oc [(x, reg_cl)] ys zs;
      let ss = stacksize () in
	Printf.fprintf oc "\tstore\t%s, %s, %d\n" reg_tmp reg_sp (ss - 4);
	Printf.fprintf oc "\taddi\t%s, %s, %d\n" reg_sp reg_sp ss;
	Printf.fprintf oc "\tload\t%s, %s, 0\n" reg_tmp (reg reg_cl);
	Printf.fprintf oc "\tmtctr\t%s\n" reg_tmp (*TODO: PCの設定*);
	Printf.fprintf oc "\tbctrl\n"; (*TODO: リンクレジスタの設定*)
	Printf.fprintf oc "\tsubi\t%s, %s, %d\n" reg_sp reg_sp ss;
	Printf.fprintf oc "\tload\t%s, %s, %d\n" reg_tmp reg_sp (ss - 4);
	(if List.mem a allregs && a <> regs.(0) then 
	   Printf.fprintf oc "\tmov\t%s, %s\n" (reg a) (reg regs.(0)) 
	 else if List.mem a allfregs && a <> fregs.(0) then 
	   Printf.fprintf oc "\tfmov\t%s, %s\n" (reg a) (reg fregs.(0)));
	Printf.fprintf oc "\tmtlr\t%s\n" reg_tmp (*TODO: リンクレジスタの設定*)
  | (NonTail(a), CallDir(Id.L(x), ys, zs)) -> 
      Printf.fprintf oc "\tmflr\t%s\n" reg_tmp; (*TODO: リンクレジスタの設定*)
      g'_args oc [] ys zs;
      let ss = stacksize () in
	Printf.fprintf oc "\tstore\t%s, %s, %d\n" reg_tmp reg_sp (ss - 4);
	Printf.fprintf oc "\taddi\t%s, %s, %d\n" reg_sp reg_sp ss;
	Printf.fprintf oc "\tcall\t%s\n" x;
	Printf.fprintf oc "\tsubi\t%s, %s, %d\n" reg_sp reg_sp ss;
	Printf.fprintf oc "\tload\t%s, %s, %d\n" reg_tmp reg_sp (ss - 4);
	(if List.mem a allregs && a <> regs.(0) then
	   Printf.fprintf oc "\tmov\t%s, %s\n" (reg a) (reg regs.(0))
	 else if List.mem a allfregs && a <> fregs.(0) then
	   Printf.fprintf oc "\tfov\t%s, %s\n" (reg a) (reg fregs.(0)));
	Printf.fprintf oc "\tmtlr\t%s\n" reg_tmp (*TODO リンクレジスタの設定*)
and g'_tail_if oc e1 e2 b bn x y = 
  let b_else = Id.genid (b ^ "_else") in
    Printf.fprintf oc "\t%s\t%s, %s, %s\n" bn x y b_else;
    let stackset_back = !stackset in
      g oc (Tail, e1);
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (Tail, e2)
and g'_tail_if_imm oc e1 e2 b bn x y = 
  let b_else = Id.genid (b ^ "_else") in
    Printf.fprintf oc "\t%s\t%s, %d, %s\n" bn x y b_else;
    let stackset_back = !stackset in
      g oc (Tail, e1);
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (Tail, e2)
and g'_non_tail_if oc dest e1 e2 b bn x y = 
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
    Printf.fprintf oc "\t%s\t%s, %s, %s\n" bn x y  b_else;
    let stackset_back = !stackset in
      g oc (dest, e1);
      let stackset1 = !stackset in
	Printf.fprintf oc "\tb\t%s\n" b_cont;
	Printf.fprintf oc "%s:\n" b_else;
	stackset := stackset_back;
	g oc (dest, e2);
	Printf.fprintf oc "%s:\n" b_cont;
	let stackset2 = !stackset in
	  stackset := S.inter stackset1 stackset2
and g'_non_tail_if_imm oc dest e1 e2 b bn x y = 
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
    Printf.fprintf oc "\t%s\t%s, %d, %s\n" bn x y  b_else;
    let stackset_back = !stackset in
      g oc (dest, e1);
      let stackset1 = !stackset in
	Printf.fprintf oc "\tb\t%s\n" b_cont;
	Printf.fprintf oc "%s:\n" b_else;
	stackset := stackset_back;
	g oc (dest, e2);
	Printf.fprintf oc "%s:\n" b_cont;
	let stackset2 = !stackset in
	  stackset := S.inter stackset1 stackset2
and g'_args oc x_reg_cl ys zs = 
  let (i, yrs) = 
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl) ys in
    List.iter
      (fun (y, r) -> Printf.fprintf oc "\tmov\t%s, %s\n" (reg r) (reg y))
      (shuffle reg_sw yrs);
    let (d, zfrs) = 
      List.fold_left
	(fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
	(0, []) zs in
      List.iter
        (fun (z, fr) -> Printf.fprintf oc "\tfmov\t%s, %s\n" (reg fr) (reg z))
	(shuffle reg_fsw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) = (*TODO: main周り以外のアセンブリの成型*)
  Format.eprintf "generating assembly...@.";
  (if data <> [] then
    (Printf.fprintf oc "\t.data\n\t.literal8\n";
     List.iter
       (fun (Id.L(x), d) ->
	 Printf.fprintf oc "\t.align 3\n";
	 Printf.fprintf oc "%s:\t # %f\n" x d;
	 Printf.fprintf oc "\t.long\t%ld\n" (gethi d);
	 Printf.fprintf oc "\t.long\t%ld\n" (getlo d))
       data));
  Printf.fprintf oc "\t.text\n";
  Printf.fprintf oc "\t.globl  _min_caml_start\n";
  Printf.fprintf oc "\t.align 2\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc "_min_caml_start: # main entry point\n";
  Printf.fprintf oc "\tmflr\tr0\n"; (*TODO: リンクレジスタの設定*)
  Printf.fprintf oc "\tstmw\tr30, -8(r1)\n"; (*TODO: stmw に変わる命令*)
  Printf.fprintf oc "\tstw\tr0, 8(r1)\n";
  Printf.fprintf oc "\tstwu\tr1, -96(r1)\n";
  Printf.fprintf oc "   # main program start\n";
  stackset := S.empty;
  stackmap := [];
  g oc (NonTail("_R_0"), e);
  Printf.fprintf oc "   # main program end\n";
(*  Printf.fprintf oc "\tmr\tr3, %s\n" regs.(0); *)
  Printf.fprintf oc "\tlwz\tr1, 0(r1)\n";
  Printf.fprintf oc "\tlwz\tr0, 8(r1)\n";
  Printf.fprintf oc "\tmtlr\tr0\n";
  Printf.fprintf oc "\tlmw\tr30, -8(r1)\n";
  Printf.fprintf oc "\tblr\n"
