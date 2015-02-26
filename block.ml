(* constructs the actual cfg           *
 * TODO: refactor and clean the code   *)
type id_or_imm = Asm.id_or_imm
type t = 
  | Nop
  | Li of int
  | FLi of Id.l 
  | SetL of Id.l (*Set Label*)
  | Mov of Id.t
  | Neg of Id.t
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | Sll of Id.t * id_or_imm
  | Srl of Id.t * id_or_imm
  | Sla of Id.t * id_or_imm
  | Sra of Id.t * id_or_imm
  | Load of Id.t * id_or_imm
  | Store of Id.t * Id.t * id_or_imm
  | Loadi of int
  | Storei of Id.t * int
  | FMov of Id.t  
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | Itof of Id.t
  | Ftoi of Id.t
  | FLoad of Id.t * id_or_imm
  | FStore of Id.t * Id.t * id_or_imm
  | FLoadi of int 
  | FStorei of Id.t * int
  | Comment of string
  (* virtual instructions *)
  | IfEq of Id.t * Id.t 
  | IfLT of Id.t * Id.t 
  | IfFEq of Id.t * Id.t
  | IfFLT of Id.t * Id.t 
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 *)
  | Restore of Id.t (* スタック変数から値を復元 *)
  | FInv of Id.t
  | FSqrt of Id.t
  | Write of Id.t
  | Xor of Id.t * Id.t
  | Jmp
  | Goto of Id.l
  | Entry
  | Ret

type stmt = {
  sid : Id.t;
  inst : t;
  use : Id.t list;
  def : (Id.t * Type.t) list;
}

type block = stmt list

type branch = Then | Else  
type edge' = branch option

let gen_sid () = Id.genid "s"
let gen_bid () = Id.genid "b"

module CFG =
struct
  module GraphType =
  struct
    type vertex = Id.t
    type v_info = block
    type edge = edge'
  end
  module CFG = Graph.Make (GraphType)
  include CFG
end

type fundef = {
  name : Id.l;
  args : Id.t list;
  fargs : Id.t list;	
  blocks : CFG.t;
  ret : Type.t;
  head : Id.t;
  storing_regs : Id.t list;		
}

type prog = Prog of (Id.l * float) list * fundef M.t

(* for debug *)

let string_of_id_or_imm = function
  | Asm.V(x) -> x
  | Asm.C(i) -> string_of_int i

let string_of_inst = function 
  | Nop -> "nop"
  | Li(i) -> "li " ^ string_of_int i
  | FLi(Id.L(l)) -> "fli " ^ l
  | SetL(Id.L(l)) -> "SetL " ^ l 
  | Mov(x) -> "mov " ^ x
  | Neg(x) -> "neg " ^ x
  | Add(x, y') -> "add " ^ x ^ ", " ^ string_of_id_or_imm y'
  | Sub(x, y') -> "sub " ^ x ^ ", " ^ string_of_id_or_imm y'
  | Sll(x, y') -> "sll " ^ x ^ ", " ^ string_of_id_or_imm y'
  | Srl(x, y') -> "srl " ^ x ^ ", " ^ string_of_id_or_imm y'
  | Sla(x, y') -> "sla " ^ x ^ ", " ^ string_of_id_or_imm y'
  | Sra(x, y') -> "sra " ^ x ^ ", " ^ string_of_id_or_imm y'
  | Load(x, y') -> "load " ^ x ^ ", " ^ string_of_id_or_imm y'
  | Store(x, y, z') -> 
     "store " ^ x ^ ", " ^ y ^ ", " ^ string_of_id_or_imm z'
  | Loadi(i) -> "loadi " ^ string_of_int i
  | Storei(x, i) -> "storei " ^ x ^ " " ^ string_of_int i
  | FMov(x) -> "fmov " ^ x
  | FNeg(x) -> "fneg " ^ x
  | FAdd(x, y) -> "fadd " ^ x ^ ", " ^ y
  | FSub(x, y) -> "fsub " ^ x ^ ", " ^ y
  | FMul(x, y) -> "fmul " ^ x ^ ", " ^ y
  | FDiv(x, y) -> "fdiv " ^ x ^ ", " ^ y
  | Itof(x) -> "itof " ^ x
  | Ftoi(x) -> "ftoi " ^ x
  | FLoad(x, y') -> "fload " ^ x ^ ", " ^ string_of_id_or_imm y'
  | FStore(x, y, z') -> 
     "fstore " ^ x ^ ", " ^ y ^ ", " ^ string_of_id_or_imm z'
  | FLoadi(i) -> "floadi " ^ string_of_int i
  | FStorei(x, i) -> "fstorei " ^ x ^ " " ^ string_of_int i
  | Comment(x) -> "#" ^ x
  | IfEq(x, y) -> x ^ " = " ^ y 
  | IfLT(x, y) -> x ^ " < " ^ y
  | IfFEq(x, y) -> x ^ " =. " ^ y 
  | IfFLT(x, y) -> x ^ " <. " ^ y
  | CallCls _ -> "callcls"  (* TODO *)
  | CallDir(Id.L(l), _, _) -> "calldir " ^ l (* TODO *)
  | Save(x, y) -> "save " ^ x ^ ", " ^ y
  | Restore(x) -> "restore " ^ x
  | FInv(x) -> "finv " ^ x
  | FSqrt(x) -> "fsqrt " ^ x
  | Write(x) -> "write " ^ x
  | Xor(x, y) -> "xor " ^ x ^ ", " ^ y
  | Jmp -> "jmp"
  | Goto(Id.L(l)) -> "goto " ^ l
  | Entry -> "entry"
  | Ret -> "ret"
	     
let string_of_stmt { sid = sid; inst = e; use = use; def = def } =
  sid ^ ": " ^
    (if def = [] 
     then ""
     else
       String.concat ", "
	 (List.map
	    (fun (x, t) -> x ^ " : " ^ Type.string_of_type t)
            def) ^
	   " := ") ^ 
      string_of_inst e

let string_of_block b stmts =
  b ^ ":\n" ^
  List.fold_left (fun res stmt -> res ^ "\n" ^ string_of_stmt stmt) "" stmts

let sanitize b = 
  let r = Str.regexp "\\." in
  Str.global_replace r "_" b

let output_for_graphviz oc cfg =
  Printf.fprintf oc "digraph cfg {\n";
  Printf.fprintf oc "node [shape=box];\n";
  CFG.fold
    (fun b block () ->
     Printf.fprintf oc "%s [label=\"%s\"];\n" (sanitize b) (String.escaped (string_of_block b block)))
     cfg ();
  CFG.fold_e
    (fun u v e () ->
     let tag = match e with None -> "" | Some(Then) -> "then" | Some(Else) -> "else"  in
     Printf.fprintf oc "%s -> %s [label=\"%s\"];\n" (sanitize u) (sanitize v) tag) cfg ();
  Printf.fprintf oc "}\n"

(* end of debug *)

let fv_id_or_imm = function Asm.V (x) -> [x] | _ -> []


let to_stmt inst dest =
  match (inst, dest) with
  | Nop, [] -> { sid = gen_sid(); inst = inst; use = []; def = dest }
  | Li _, [_] -> { sid = gen_sid(); inst = inst; use = []; def = dest }
  | FLi _, [_, Type.Float] -> { sid = gen_sid(); inst = inst; use = []; def = dest } 
  | SetL _, [_] -> { sid = gen_sid(); inst = inst; use = []; def = dest }
  | Mov(x), [_] | Neg(x), [_] | Itof(x), [_] | Write(x), [_] -> 
    { sid = gen_sid(); inst = inst; use = [x]; def = dest }
  | Write(x), ([] | [_, Type.Unit]) ->  { sid = gen_sid(); inst = inst; use = [x]; def = []}
  | Add(x, y'), [_] | Sub(x, y'), [_] -> 
    { sid = gen_sid(); inst = inst; use = x :: fv_id_or_imm y'; def = dest } 
  | Xor(x, y), [_] -> { sid = gen_sid(); inst = inst; use = [x; y]; def = dest }
  | Sll(x, y'), [_] | Srl(x, y'), [_] | Sla(x, y'), [_] | Sra(x, y'), [_] -> 
    { sid = gen_sid(); inst = inst; use = x :: fv_id_or_imm y'; def = dest }
  | Load (x, y'), [_] -> 
    { sid = gen_sid(); inst = inst; use = x :: fv_id_or_imm y'; def = dest }
  | Store (x, y, z'), ([] | [_,Type.Unit]) | FStore(x, y, z'), ([] | [_, Type.Unit]) -> 
    { sid = gen_sid (); inst = inst; use = x :: y :: fv_id_or_imm z'; def = [] }
  | Loadi _, [_] -> { sid = gen_sid(); inst = inst; use = []; def = dest }
  | Storei(x, _), ([] | [_, Type.Unit]) | FStorei(x, _), ([] | [_, Type.Unit]) -> 
    { sid = gen_sid(); inst = inst; use = [x]; def = [] }
  | FMov(x), [_, Type.Float] | FNeg(x), [_, Type.Float] | Ftoi(x), [_, Type.Float] 
  | FInv(x), [_, Type.Float] | FSqrt(x), [_, Type.Float]  -> 
    { sid = gen_sid(); inst = inst; use = [x]; def = dest }
  | FAdd(x, y), [_, Type.Float] | FSub(x, y), [_, Type.Float] 
  | FMul(x, y), [_, Type.Float] | FDiv(x, y), [_, Type.Float] -> 
    { sid = gen_sid(); inst = inst; use = [x; y]; def = dest }
  | FLoad(x, y'), [_, Type.Float] -> 
     { sid = gen_sid(); inst = inst; use =  x :: fv_id_or_imm y'; def = dest }
  | FLoadi _, [_, Type.Float] -> 
    { sid = gen_sid(); inst = inst; use = []; def = dest }
  | Itof(x), [_, Type.Float] ->
     { sid = gen_sid(); inst = inst; use = [x]; def = dest }
  | Ftoi(x), [_] -> 
     { sid = gen_sid(); inst = inst; use = [x]; def = dest }
  | IfEq(x, y) , [] | IfLT(x, y), [] | IfFEq(x, y), [] | IfFLT(x, y), [] ->
     { sid = gen_sid(); inst = inst; use = [x; y]; def = dest }
  | CallCls (_, xs, ys), _ | CallDir(_, xs, ys), _  -> 
     { sid = gen_sid(); inst = inst ; use = xs @ ys; def = dest }
  | Save(x, y), [] -> { sid = gen_sid(); inst = inst; use = [x]; def = [] } (*TODO*)
  | Restore(x), [_] -> { sid = gen_sid(); inst = inst; use = []; def = dest } (*TODO*)
  | Jmp, [] -> { sid = gen_sid(); inst = inst; use = []; def = dest }
  | Ret, [] -> { sid = gen_sid(); inst = inst; use = []; def = dest } 
  | Goto(l), [] -> { sid = gen_sid(); inst = inst; use = []; def = dest }
  | Comment _, [] -> { sid = gen_sid(); inst = inst; use = []; def = dest } 
  | inst, _ -> Printf.fprintf stderr "%s\n" (string_of_inst inst);
	       List.iter 
		 (fun (x, t) -> Printf.fprintf stderr "%s : %s" x (Type.string_of_type t))
		 dest;
	       assert false 

(* used in gColoring.ml *)
let replace_stmt env stmt =
  let f x = if M.mem x env then M.find x env else x in
  let f' = function Asm.V x -> Asm.V (f x) | c -> c in
  let stmt = { stmt with use = List.map f stmt.use } in
  let inst = match stmt.inst with
    | Mov(x) -> Mov(f x)
    | Neg(x) -> Neg(f x)
    | Add(x, y') -> Add(f x, f' y')
    | Sub(x, y') -> Sub(f x, f' y')
    | Sll(x, y') -> Sll (f x, f' y')
    | Srl(x, y') -> Srl (f x, f' y')
    | Sla(x, y') -> Sla (f x, f' y')
    | Sra(x, y') -> Sra (f x, f' y')
    | Load(x, y') -> Load(f x, f' y')
    | Store(x, y, z') -> Store(f x, f y, f' z')
    | Storei(x,i) -> Storei(f x, i)
    | FMov(x) -> FMov(f x)
    | FNeg(x) -> FNeg(f x)
    | FAdd(x, y) -> FAdd(f x, f y)
    | FSub(x, y) -> FSub(f x, f y)
    | FMul(x, y) -> FMul(f x, f y)
    | FDiv(x, y) -> FDiv(f x, f y)
    | FInv(x) -> FInv(f x)
    | FSqrt(x) -> FSqrt(f x)
    | Itof(x) -> Itof(f x)
    | Ftoi(x) -> Ftoi(f x)
    | FLoad(x, y') -> FLoad(f x, f' y')
    | FStore(x, y, z') -> FStore(f x, f y, f' z')
    | FStorei(x,i) -> FStorei(f x, i)   
    | IfEq(x, y) -> IfEq(f x, f y)
    | IfLT(x, y) -> IfLT(f x, f y)
    | IfFEq(x, y) -> IfFEq(f x, f y)
    | IfFLT(x, y) -> IfFLT(f x, f y)
    | CallCls(x, ys, zs) -> CallCls(f x, List.map f ys, List.map f zs)
    | CallDir(l, xs, ys) -> CallDir(l, List.map f xs, List.map f ys)
    | Save(x, y) -> Save(f x, y) (* TODO *)
    | Restore(x) -> Restore(x)  (* TODO *)
    | Write(x) -> Write(f x)
    | Xor(x, y) -> Xor(f x, f y)
    | e -> e
  in
    { stmt with inst = inst }

let replace_stmt' env stmt =
  let stmt = replace_stmt env stmt in
  let stmt = { stmt with def = List.map (fun (x, t) -> if M.mem x env then (M.find x env, t) else (x, t)) stmt.def } in
  stmt

let null = "NULL"
let isnull v = "NULL" = v 
let add_block stmts g =
  let b = gen_bid () in
  (b, CFG.add_v b stmts g)    
let concat_block stmts v g =
  if isnull v then add_block stmts g 
  else
    (assert(CFG.mem v g);
     let stmts' = CFG.find v g in
     let (branch, stmts')  = match List.rev stmts' with
       | [] -> assert false
       | st :: sts -> (st,  List.rev sts)
     in 
     match branch with
     | { inst = Jmp } -> (v, CFG.add_v v (stmts' @ stmts) g)
     | _ ->
	let (v', g') = add_block stmts g in
	let g'' = CFG.add_e v v' None g' in
	(v', g''))
let concat_stmts stmts =
  concat_block (stmts @ [to_stmt Jmp []])


(***** map and fold *****)
let map_stmt (f : stmt -> stmt) g = CFG.map (fun v -> List.map (fun stmt -> f stmt)) g
let fold_stmt (f : stmt -> 'a -> 'a) g init = CFG.fold (fun _ stmts acc -> List.fold_left (fun acc stmt -> f stmt acc) acc stmts) g init
let map_stmt_list (f : stmt -> stmt list) g = CFG.map (fun v stmts -> List.fold_left (fun acc stmt -> acc @ f stmt) [] stmts) g
let map_func f (Prog(xs, fundefs)) =
  Prog(xs, M.mapi (fun l func -> f l func) fundefs)
let map_cfg f prog =
  map_func (fun l func -> { func with blocks = f l func.blocks }) prog

let fv func =
  CFG.fold
    (fun b ->
     List.fold_right
       (fun { use = use; def = def } ->
        S.union (S.union (S.of_list use) (S.of_list (List.map fst def)))))
    func.blocks S.empty
    

(*改良の余地あり?*)
let rec shuffle sw xys = 
  let (_, xys) = List.partition (fun (x, y) -> x = y) xys in
    match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
      | ([], []) -> []
      | ((x, y) :: xys, []) -> 
	 (y, sw) :: (x, y) :: 
	   shuffle sw (List.map (function 
				  | (y', z) when y = y' -> (sw, z)
				  | yz -> yz) xys)
      | (xys, acyc) -> acyc @ shuffle sw xys
let move_args pars args =
  assert (List.length pars = List.length args);
  let swap = Id.genid "swap" in
  let xys = List.fold_right2 (fun par arg xs -> (par, arg) :: xs) pars args [] in
  List.fold_right 
    (fun (par, arg) xs -> xs @ [to_stmt (Mov(arg)) [par, Type.Int]]) 
    (shuffle swap xys) []
let move_fargs pars args =
  assert (List.length pars = List.length args);
  let swap = Id.genid "swap" in
  let xys = List.fold_right2 (fun par arg xs -> (par, arg) :: xs) pars args [] in
  List.fold_right 
    (fun (par, arg) xs -> xs @ [to_stmt (FMov(arg)) [par, Type.Float]]) 
    (shuffle swap xys) []
(*let move_args pars args =  
  assert (List.length pars = List.length args);
  List.fold_right2 (fun par arg xs -> if par == arg then xs else xs @ [to_stmt (Mov(arg)) [par, Type.Int]]) pars args []*)
(*let move_fargs pars args =
  assert (List.length pars = List.length args);
  List.fold_right2 (fun par arg xs -> if par == arg then xs else xs @ [to_stmt (FMov(arg)) [par, Type.Float]]) pars args []*)
let move_args_with_type xs ys t = 
  match t with 
  | Type.Float -> move_fargs xs ys
  | _ -> move_args xs ys 


let rec remove_unreachable head cfg  =
  let reachables = CFG.reachables head cfg in
  CFG.fold 
    (fun v _ cfg -> if List.mem v reachables then
		      cfg
		    else 
		      CFG.remove_v v cfg)
    cfg cfg




(* env : Id.t -> Type.t *)
let rec g data fname body  = 
  let rec h env v cfg dest tail e  = 
    let concat_asm_exp exp = 
      concat_stmts [to_stmt exp dest] v cfg in
    match e with
    | Asm.Ans(e') -> 
       (match e' with
	| Asm.Nop -> (v, cfg)
	| Asm.Li(i) -> concat_asm_exp (Li(i)) 
	| Asm.FLi(l) -> concat_asm_exp (FLi(l)) 
	| Asm.SetL(l) -> concat_asm_exp (SetL(l))
	| Asm.Mov(x) -> concat_asm_exp (Mov(x)) 
	| Asm.Neg(x) -> concat_asm_exp (Neg(x))
	| Asm.Add(x, y') -> concat_asm_exp (Add(x, y'))
	| Asm.Sub(x, y') -> concat_asm_exp (Sub(x, y'))
	| Asm.Sll(x, y') -> concat_asm_exp (Sll(x, y'))
	| Asm.Srl(x, y') -> concat_asm_exp (Srl(x, y'))
	| Asm.Sla(x, y') -> concat_asm_exp (Sla(x, y'))
	| Asm.Sra(x, y') -> concat_asm_exp (Sra(x, y'))
	| Asm.FMov(x) -> concat_asm_exp (FMov(x))
	| Asm.FNeg(x) -> concat_asm_exp (FNeg(x))
	| Asm.FAdd(x, y) -> concat_asm_exp (FAdd(x, y))
	| Asm.FSub(x, y) -> concat_asm_exp (FSub(x, y))
	| Asm.FMul(x, y) -> concat_asm_exp (FMul(x, y))
	| Asm.FDiv(x, y) -> concat_asm_exp (FDiv(x, y))
	| Asm.Itof(x) -> concat_asm_exp (Itof(x))
	| Asm.Ftoi(x) -> concat_asm_exp (Ftoi(x))
	| Asm.FInv(x) -> concat_asm_exp (FInv(x))
	| Asm.FSqrt(x) -> concat_asm_exp (FSqrt(x))
	| Asm.Write(x) -> concat_asm_exp (Write(x))
	| Asm.Xor(x, y) -> concat_asm_exp (Xor(x, y))
	| Asm.Load(x, y') -> concat_asm_exp (Load(x, y'))
	| Asm.FLoad(x, y') -> concat_asm_exp (FLoad(x, y'))
	| Asm.Store(x, y, z') -> concat_asm_exp (Store(x, y, z'))
	| Asm.FStore(x, y, z') -> concat_asm_exp (FStore(x, y, z'))
	| Asm.Loadi(i) -> concat_asm_exp (Loadi(i))
	| Asm.FLoadi(i) -> concat_asm_exp (Loadi(i)) 
	| Asm.Storei(x, i) -> concat_asm_exp (Storei(x, i))
	| Asm.FStorei(x, i) -> concat_asm_exp (Storei(x, i))
	| Asm.Save(x, y) -> concat_asm_exp (Save(x, y))
	| Asm.Restore(x) -> concat_asm_exp (Restore(x))
	| Asm.IfEq(x, y, e1, e2) ->
	   concat_if env v cfg dest tail e1 e2 (IfEq(x, y))
	| Asm.IfLT(x, y, e1, e2) ->
	   concat_if env v cfg dest tail e1 e2 (IfLT(x, y))
	| Asm.IfFEq(x, y, e1, e2) ->
	   concat_if env v cfg dest tail e1 e2 (IfFEq(x, y))
	| Asm.IfFLT(x, y, e1, e2) ->
	   concat_if env v cfg dest tail e1 e2 (IfFLT(x, y))
	| Asm.CallDir (Id.L(l), args, fargs) when not (M.mem l data) -> (*external function call*)
	    concat_asm_exp (CallDir(Id.L(l), args, fargs))
        | Asm.CallDir (Id.L(l), args, fargs) -> (* TODO : change tail call to jump*)
           let func = assert (if M.mem l data then true else (Printf.fprintf stderr "%s\n" l; false)); 
		      M.find l data in
           (match () with
             | _ when tail && l = fname -> (* tail recursion *)
                let (v', cfg') = concat_stmts (move_args func.args args) v cfg in
		let (v'', cfg'') = concat_stmts (move_fargs func.fargs fargs) v' cfg' in
		let after_head = List.hd (CFG.succs func.head cfg'') in
                 let cfg3 = CFG.add_e v'' after_head None cfg'' in (* jump to the next block of the head of the function *)
                   (null, cfg3)
	     | _  when tail -> (* tail call, but not tail recursion *)
		                            (* TODO: 呼び出し規約に関する操作をここで書くのは汚い                   　　　 *)   
		let n = List.length args in (* 外部関数呼び出しでも同じ考えが使われているので統合する. Goto(l, args, fargs)*)
		let m = List.length fargs in(* とかにして,gRegallocの関数で統一的に扱いたい                                *)
		let rec take n xs =  (* TODO: 自作の標準ライブラリかリスト用のmoduleほしい. std.ml? l.ml? *)
		  (match (n, xs) with
		      | 0, _ -> []
		      | _, [] -> []
		      | n, x::xs -> x :: take (n - 1) xs) 
		in
		let actual_args = take n Asm.allregs in
		let actual_fargs = take m Asm.allfregs in
		let init_stmt = 
		     (move_args actual_args args) @ (move_fargs actual_fargs fargs)
		in
		(*let (v', cfg') = concat_stmts (move_args func.args args) v cfg in
		let (v'', cfg'') = concat_stmts (move_fargs func.fargs fargs) v' cfg' in*)
		let v3 = gen_bid() in
		let cfg3 = CFG.add_v v3 (init_stmt @ [to_stmt (Goto(Id.L(l))) []])  cfg in
		let cfg4 = CFG.add_e v v3 None cfg3 in
		(null, cfg4)
             | _ -> concat_asm_exp (CallDir(Id.L(l), args, fargs)))
	| Asm.CallCls _ -> Printf.fprintf stderr "TODO\n"; assert false
	| Asm.Comment _ -> Printf.fprintf stderr "TODO\n"; assert false
       )
    | Asm.Let((x, t), e1', e2) -> 
       let (v', cfg') = h env v cfg [x, t] false (Asm.Ans(e1')) in
       let env' = M.add x t env in
       h env' v' cfg' dest tail e2 
       
	 
  and concat_if env v cfg dest tail e1 e2 ifexp =
    let (v, cfg') = concat_block [to_stmt ifexp []] v cfg in
    let (v1, cfg1) = concat_stmts [] v cfg' in 
    let cfg'' = CFG.add_e v v1 (Some Then) cfg1 in
    let (v2, cfg2) = concat_stmts [] v cfg'' in
    let cfg''' = CFG.add_e v v2 (Some Else) cfg2 in
    let (v1', cfg1') = h env v1 cfg''' dest tail e1 in
    let (v2', cfg2') = h env v2 cfg1' dest tail e2 in
    
    let (v_cong, cfg4) = add_block [to_stmt Jmp []] cfg2' in 

    let cfg5 = if isnull v1' then cfg4 else CFG.add_e v1' v_cong None cfg4 in
    let cfg6 = if isnull v2' then cfg5 else CFG.add_e v2' v_cong None cfg5 in

    (*let cfg7 = CFG.add_e v (List.hd (CFG.succs v1 cfg6)) (Some Then) cfg6 in
    let cfg8 = CFG.add_e v (List.hd (CFG.succs v2 cfg7)) (Some Else) cfg7 in*)
    (*let cfg9 = CFG.remove_v v1 cfg8 in
    let cfg10 = CFG.remove_v v2 cfg9 in*)
      (v_cong, cfg6) (*cfg10*)

  in 
  let func = assert (if M.mem fname data then true else (Printf.fprintf stderr "%s\n" fname; false)); 
	     M.find fname data in
  let head = func.head in 
  let cfg = CFG.add_v head
	    ([{ sid = gen_sid ();
	       inst = Entry; 
	       use = [];
	       def =  ["$r00", Type.Int; "$f00", Type.Float] @
			(List.map (fun x -> (x, Type.Int)) func.args) @ (List.map (fun x -> (x, Type.Float)) func.fargs) }
	     ] @
	       [to_stmt Jmp []])
	    func.blocks  
  in
  let b = gen_bid() in
  let cfg = CFG.add_v b [to_stmt Jmp []] cfg in
  let cfg = CFG.add_e head b None cfg in
  let dest = 
    match func.ret with 
      Type.Unit -> [] 
    | _ -> [fname ^ "_ret", func.ret]
  in
  h M.empty b cfg dest true body


(* main routine Asm.Prog -> Block.Prog *)
let f (Asm.Prog(xs, ys, e)) = 
  let (data, body_data) = 
    List.fold_left 
      (fun (data, body_data) { Asm.name = Id.L(l); Asm.args = xs; Asm.fargs = ys; Asm.body = e; Asm.ret = ret } -> 
       let data' = M.add 
		     l
		     { name = Id.L(l);
		       args = xs; 
		       fargs = ys; 
		       blocks = CFG.empty;
		       ret = ret; 
		       head = l ^ "_entry";
		       storing_regs = [] }
		   data
       in
     let body_data' = M.add l e body_data in
     (data', body_data'))
      (M.empty, M.empty)
      ys
  in 
  let data = M.add "__main__" 
		   { name = Id.L("__main__"); 
		     args = [];
		     fargs = []; 
		     blocks = CFG.empty;
		     ret = !Typing.ret_type; 
		     head = "__main___entry";
		     storing_regs = [] } 
		   data 
  in
  let data = M.mapi 
    (fun fname  fundef ->
     let fbody = if fname = "__main__" 
		 then e
		 else (assert (if M.mem fname body_data then true else (Printf.fprintf stderr "%s\n" fname; false));  
		       M.find fname body_data) in
     let (v, cfg) = g data fname fbody in
     let (_, cfg') = concat_block 
		       [{ sid = gen_sid ();
			  inst = Ret; 
			  use = (match  fundef.ret with Type.Unit -> [] | _ -> [fname ^ "_ret"]);
			  def = [] }]
		       v cfg 
     in
     let cfg'' = remove_unreachable fundef.head cfg' in
     { fundef with  blocks = cfg'' })
    data 
  in
  Prog(xs, data)



