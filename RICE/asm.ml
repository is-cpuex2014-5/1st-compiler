(* RICE assembly with a few virtual instructions *)

type id_or_imm = V of Id.t | C of int
type t = (* ̿����� *)
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp = (* ��İ�Ĥ�̿����б����뼰 *) (*ȴ���Ƥ���̿���¿��*) (*��������̿�᤬���ä���ʤ�*)
  | Nop
  | Li of int
  | FLi of Id.l (*ɬ�פʤΤ�*)
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
  | FMov of Id.t  (*ftoi, itof���ʤ�����*)
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | FLoad of Id.t * Id.t * id_or_imm
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
  | Save of Id.t * Id.t (* �쥸�����ѿ����ͤ򥹥��å��ѿ�����¸ *)
  | Restore of Id.t (* �����å��ѿ������ͤ����� *)
type fundef =
    { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
(* �ץ�������� = ��ư���������ơ��֥� + �ȥåץ�٥�ؿ� + �ᥤ��μ� *)
type prog = Prog of (Id.l * float) list * fundef list * t

(* shorthand of Let for float *)
(* fletd : Id.t * exp * t -> t *)
let fletd (x, e1, e2) = Let ((x, Type.Float), e1, e2)
(* shorthand of Let for unit *)
(* seq : exp * t -> t *)
let seq (e1, e2) = Let ((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = Array.init 12 (fun i -> match i with 
				   | m when m < 9 -> Printf.sprintf "%%r0%d" (i+1)
				   | _ -> Printf.sprintf "%%r%d" (i+1))
let fregs = Array.init 15 (fun i -> match i with 
				   | m when m < 9 -> Printf.sprintf "%%f0%d" (i+1)
				   | _ -> Printf.sprintf "%%f%d" (i+1))
let allregs = Array.to_list regs
let allfregs = Array.to_list fregs
let reg_cl = regs.(Array.length regs - 1) (* closure address *)
let reg_sw = regs.(Array.length regs - 2) (* temporary for swap *)
let reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *)
let reg_hp = "%r13"
let reg_sp = "%r14"


(* is_reg : Id.t -> bool *)
let is_reg x = x.[0] = '%'

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
  | Nop | Li (_) | FLi (_) | SetL (_) | Comment (_) | Restore (_) -> []
  | Mov (x) | Neg (x) | FMov (x) | FNeg (x) | Save (x, _) -> [x]
  | Add (x, y, z') -> x :: y :: fv_id_or_imm z'
  | Sub (x, y) -> [x;y] 
  | Sll (x, y') | Srl (x, y') |Sla (x, y') |Sra (x, y') -> 
      x :: fv_id_or_imm y'
  | FAdd (x, y) | FSub (x, y) | FMul (x, y) | FDiv (x, y) ->
      [x; y]
  | Store (x, y, z') | FStore (x, y, z') | FLoad (x, y, z') | Load (x, y, z') -> 
     x :: y :: fv_id_or_imm z'
  | IfEq (x, y', e1, e2) | IfLE (x, y', e1, e2) | IfGE (x, y', e1, e2) -> 
      x :: fv_id_or_imm y' @ remove_and_uniq S.empty (fv e1 @ fv e2)
  | IfFEq (x, y, e1, e2) | IfFLE (x, y, e1, e2) ->
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
let align i = if i mod 8 = 0 then i else i + 4
