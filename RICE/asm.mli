type id_or_imm = V of Id.t | C of int
type t = 
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp = (* 一つ一つの命令に対応する式 *) (*抜けている命令も多い*) (*現状論理命令がごっそりない*)
  | Nop
  | Li of int
  | FLi of Id.l (*必要なのか*)
  | SetL of Id.l (*Set Label*)
  | Mov of Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t * id_or_imm
  | Sub of Id.t * Id.t
  | Sll of Id.t * id_or_imm
  | Srl of Id.t * id_or_imm
  | Sla of Id.t * id_or_imm
  | Sra of Id.t * id_or_imm
  | Load of Id.t * Id.t *  id_or_imm
  | Store of Id.t * Id.t * id_or_imm
  | FMov of Id.t  (*ftoi, itofがない状態*)
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | Fload of Id.t * Id.t * id_or_imm
  | FStore of Id.t * Id.t * id_or_imm
  | Comment of string
  (* virtual instructions *)
  | IfEq of Id.t * id_or_imm * t * t
  | IfLE of Id.t * id_or_imm * t * t
  | IfGE of Id.t * id_or_imm * t * t
  | IfFEq of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 *)
  | Restore of Id.t (* スタック変数から値を復元 *)
type fundef =
    { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
type prog = Prog of (Id.l * float) list * fundef list * t

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
val is_reg : Id.t -> bool

val fv : t -> Id.t list
val concat : t -> Id.t * Type.t -> t -> t

val align : int -> int
