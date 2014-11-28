(* RICE assembly with a few virtual instructions *)

type id_or_imm = V of Id.t | C of int
type t = (* 命令の列 *)
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp = (* 一つ一つの命令に対応する式 *) (*抜けている命令も多い*) (*現状論理命令がごっそりない*)
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
  | FMov of Id.t  (*ftoi, itofがない状態*)
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
  | IfEq of id_or_imm * id_or_imm * t * t
  | IfLT of id_or_imm * id_or_imm * t * t
  | IfFEq of Id.t * Id.t * t * t
  | IfFLT of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 *)
  | Restore of Id.t (* スタック変数から値を復元 *)
type fundef =
    { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
(* プログラム全体 = 浮動小数点数テーブル + トップレベル関数 + メインの式 *)
type prog = Prog of (Id.l * float) list * fundef list * t

(* shorthand of Let for float *)
(* fletd : Id.t * exp * t -> t *)
let fletd (x, e1, e2) = Let ((x, Type.Float), e1, e2)
(* shorthand of Let for unit *)
(* seq : exp * t -> t *)
let seq (e1, e2) = Let ((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = Array.init 10 (fun i -> match i with 
				   | m when m < 9 -> Printf.sprintf "$r0%d" (i+1)
				   | _ -> Printf.sprintf "$r%d" (i+1))
let fregs = Array.init 15 (fun i -> match i with 
				   | m when m < 9 -> Printf.sprintf "$f0%d" (i+1)
				   | _ -> Printf.sprintf "$f%d" (i+1))
let allregs = Array.to_list regs
let allfregs = Array.to_list fregs
let reg_cl = regs.(Array.length regs - 1) (* closure address *)
let reg_sw = regs.(Array.length regs - 2) (* temporary for swap *)
let reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *)
let reg_tmp = "$r11"
let cnt_reg = "$r12"
let reg_hp = "$r13"
let reg_sp = "$r14"
let pc = "$r15"


(* is_reg : Id.t -> bool *)
let is_reg x = x.[0] = '$'

(* remove_and_uniq : S.t -> Id.t list -> Id.t list *)
let rec remove_and_uniq xs = function 
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) *)
(* fv_id_or_imm : id_or_imm -> Id.t list *)
let fv_id_or_imm = function V (x) -> [x] | _ -> []
(* fv_exp : Id.t list -> t -> S.t list *)
let rec fv_exp = function
  | Nop | Li (_) | FLi (_) | SetL (_) | Loadi (_) | FLoadi (_) |
  Comment (_) | Restore (_) -> []
  | Mov (x) | Neg (x) | Storei (x, _) | Itof(x) | Ftoi(x) |
  FMov (x) | FNeg (x) |  FStorei (x, _) | Save (x, _) -> [x]
  | Add (x, y') | Sub (x, y') -> x ::  fv_id_or_imm y'
  | Sll (x, y') | Srl (x, y') |Sla (x, y') |Sra (x, y') |
  Load (x, y') | FLoad (x, y') ->  
   x :: fv_id_or_imm y'
  | FAdd (x, y) | FSub (x, y) | FMul (x, y) | FDiv (x, y) ->
      [x; y]
  | Store (x, y, z') | FStore (x, y, z') -> x :: y :: fv_id_or_imm z'
  | IfEq (x', y', e1, e2) | IfLT (x', y', e1, e2)  -> 
      (fv_id_or_imm x') @ fv_id_or_imm y' @ remove_and_uniq S.empty (fv e1 @ fv e2)
  | IfFEq (x, y, e1, e2) | IfFLT (x, y, e1, e2) ->
      x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2)
  | CallCls (x, ys, zs) -> x :: ys @ zs
  | CallDir (_, ys, zs) -> ys @ zs
and fv = function 
  | Ans (exp) -> fv_exp exp
  | Let ((x, t), exp, e) ->
      fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)

(* fv : t -> Id.t list *)
let fv e = remove_and_uniq S.empty (fv e)

(* concat : t -> Id.t * Type.t -> t -> t *)
let rec concat e1 xt e2 = match e1 with
  | Ans (exp) -> Let (xt, exp, e2)
  | Let (yt, exp, e1') -> Let (yt, exp, concat e1' xt e2)

(* align : int -> int *)
let align i = i (*if i mod 8 = 0 then i else i + 4*)

let rec indent oc =function
  | 0 -> ()
  | n -> Printf.fprintf oc "  "; indent oc (n-1)


let rec print_exp n oc e = 
  indent oc n;
  match e with
      | Nop -> Printf.fprintf oc "Nop\n"
      | Li(i) -> Printf.fprintf oc "Li %d\n" i 
      | FLi(L(l)) -> Printf.fprintf oc "Fli %s\n" l 
      | SetL(L(l)) -> Printf.fprintf oc "SetL %s\n" l
      | Mov(s) ->  Printf.fprintf oc "Mov %s\n" s
      | Neg(s) ->  Printf.fprintf oc "Neg %s\n" s
      | Add(x, V(y)) ->  Printf.fprintf oc "Add %s, %s\n" x y
      | Add(x, C(i)) ->  Printf.fprintf oc "Add %s, %d\n" x i
      | Sub(x, V(y)) ->  Printf.fprintf oc "Sub %s, %s\n" x y
      | Sub(x, C(i)) ->  Printf.fprintf oc "Sub %s, %d\n" x i
      | Sll(x, V(y)) ->  Printf.fprintf oc "Sll %s, %s\n" x y
      | Sll(x, C(i)) ->  Printf.fprintf oc "Sll %s, %d\n" x i
      | Srl(x, V(y)) ->  Printf.fprintf oc "Srl %s, %s\n" x y
      | Srl(x, C(i)) ->  Printf.fprintf oc "Srl %s, %d\n" x i
      | Sla(x, V(y)) ->  Printf.fprintf oc "Sla %s, %s\n" x y
      | Sla(x, C(i)) ->  Printf.fprintf oc "Sla %s, %d\n" x i
      | Sra(x, V(y)) ->  Printf.fprintf oc "Sra %s, %s\n" x y
      | Sra(x, C(i)) ->  Printf.fprintf oc "Sra %s, %d\n" x i
      | Load(x, V(y)) ->  Printf.fprintf oc "Load %s, %s\n" x y
      | Load(x, C(i)) ->  Printf.fprintf oc "Load %s, %d\n" x i
      | Store(x, y, V(z)) ->  Printf.fprintf oc "Store %s, %s, %s\n" x y z
      | Store(x, y, C(i)) ->  Printf.fprintf oc "Store %s, %s,  %d\n" x y i
      | FMov(x) -> Printf.fprintf oc "Fmov %s\n" x
      | FNeg(x) -> Printf.fprintf oc "FNeg %s\n" x
      | FAdd (x, y) -> Printf.fprintf oc "FAdd %s, %s\n" x y
      | FSub (x, y) -> Printf.fprintf oc "FSub %s, %s\n" x y
      | FMul (x, y) -> Printf.fprintf oc "FMul %s, %s\n" x y
      | FDiv (x, y) -> Printf.fprintf oc "FDiv %s, %s\n" x y
      | Itof(x) -> Printf.fprintf oc "Itof %s\n" x
      | Ftoi(x) -> Printf.fprintf oc "Ftoi %s\n" x
      | FLoad(x, V(y)) ->  Printf.fprintf oc "FLoad %s, %s\n" x y
      | FLoad(x, C(i)) ->  Printf.fprintf oc "FLoad %s, %d\n" x i
      | FStore(x, y, V(z)) ->  Printf.fprintf oc "FStore %s, %s, %s\n" x y z
      | FStore(x, y, C(i)) ->  Printf.fprintf oc "FStore %s, %s,  %d\n" x y i
      | Comment(s) -> Printf.fprintf oc "#%s\n" s
      | IfEq (V(x), V(y), e1, e2) -> Printf.fprintf oc "IfEq %s %s\n" x y; print_t (n+1) oc e1; print_t (n+1) oc e2
      | IfEq (V(x), C(i), e1, e2) -> Printf.fprintf oc "IfEq %s %d\n" x i; print_t (n+1) oc e1; print_t (n+1) oc e2
      | IfLT (V(x), V(y), e1, e2) -> Printf.fprintf oc "IfLT %s %s\n" x y; print_t (n+1) oc e1; print_t (n+1) oc e2
      | IfLT (V(x), C(i), e1, e2) -> Printf.fprintf oc "IfLT %s %d\n" x i; print_t (n+1) oc e1; print_t (n+1) oc e2
      | IfFEq (x, y, e1, e2) -> Printf.fprintf oc "IfFEq %s %s\n" x y; print_t (n+1) oc e1; print_t (n+1) oc e2
      | IfFLT (x, y, e1, e2) -> Printf.fprintf oc "IfFLT %s %s\n" x y; print_t (n+1) oc e1; print_t (n+1) oc e2
      | CallCls(x, is, fs) -> Printf.fprintf oc "Callcls %s, args : " x; List.iter (fun x -> Printf.fprintf oc "%s " x) is;  List.iter (fun x -> Printf.fprintf oc "%s " x) fs;  Printf.fprintf oc "\n"
      | CallDir(L(l), is, fs) -> Printf.fprintf oc "Callcls %s, args : " l;  List.iter (fun x -> Printf.fprintf oc "%s " x) is;  List.iter (fun x -> Printf.fprintf oc "%s " x) fs;  Printf.fprintf oc "\n"
      | _ -> Printf.fprintf oc "will be implemented\n"						
  and print_t n oc e = 
    indent oc n;
    match e with 
    | Ans(e') -> print_exp n oc e'
    | Let ((x, t), e1, e') -> Printf.fprintf oc "Let %s\n" x;
			      print_exp (n + 1) oc e1 ;
			      print_t (n + 1) oc e' 
let print_fundef oc fdef = 
  Printf.fprintf oc "Name : %s\n" (match fdef.name with L(l) -> l) ;
  Printf.fprintf oc "Args "; List.iter (fun x -> Printf.fprintf oc "%s " x) fdef.args;  List.iter (fun x -> Printf.fprintf oc "%s " x) fdef.fargs; Printf.fprintf oc "\n";
  Printf.fprintf oc "Body "; print_t 0 oc fdef.body 
  
  
let p oc e = 
  print_t 0 oc e; e
let p' oc e = 
  match e with
    Prog(_,fdefs, e')as p ->
    List.iter (fun x ->print_fundef oc x) fdefs; 
    print_t 0 oc e'; 
    p
    
