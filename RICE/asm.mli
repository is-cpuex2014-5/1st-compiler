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
  | IfEq of Id.t * Id.t * t * t
  | IfLT of Id.t * Id.t * t * t
  | IfFEq of Id.t * Id.t * t * t
  | IfFLT of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 *)
  | Restore of Id.t (* スタック変数から値を復元 *)
  | FInv of Id.t
  | FSqrt of Id.t
  | Write of Id.t
  | Xor of Id.t * Id.t
type fundef =
    { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
type prog = Prog of (Id.l * float) list * fundef list * t

val invflag : bool ref
val sqrtflag : bool ref

val fletd : Id.t * exp * t -> t (* shorthand of Let for float *)
val seq : exp * t -> t (* shorthand of Let for unit *)

val regs : Id.t array
val fregs : Id.t array
val allregs : Id.t list
val allfregs : Id.t list
val reg_cl : Id.t
val reg_sw : Id.t
val reg_fsw : Id.t
val reg_hp : Id.t
val reg_sp : Id.t
val reg_tmp : Id.t
val cnt_reg : Id.t
val pc : Id.t

val is_reg : Id.t -> bool
val reg_type_of : Id.t -> Type.t
val ret_reg_of : Type.t -> Id.t

val fv : t -> Id.t list
val concat : t -> Id.t * Type.t -> t -> t

val align : int -> int

val p : out_channel -> t -> t
val p' : out_channel -> prog -> prog
