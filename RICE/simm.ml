open Asm

let rec g env = function (* 命令列の 16 bit 即値最適化 *)
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t), Li(i), e) when (-32768 <= i) && (i < 32768) ->
      let e' = g (M.add x i env) e in
      if List.mem x (fv e') then Let((x, t), Li(i), e') else e'
  | Let(xt, Sll(y, C(i)), e) when M.mem y env -> (* for array access *)
      g env (Let(xt, Li((M.find y env) lsl i), e))
  | Let(xt, exp, e) -> Let(xt, g' env exp, g env e)
and g' env = function (* 各命令の 16 bit 即値最適化 *)
  | Add(x, y, C(i)) when M.mem y env && (let j = M.find y env + i in (-4096 <= j) && (j < 4096)) ->
     Add(x, "%r0", C(M.find y env + i))
  | Add(x, y, C(i)) when M.mem x env && (let j = M.find x env + i in (-4096 <= j) && (j < 4096)) -> 
     Add(y, "%r0", C(M.find x env + i))
  (*| Sub(x, V(y)) when M.mem y env -> Sub(x, C(M.find y env))*)
  | Sll(x, V(y)) when M.mem y env -> Sll(x, C(M.find y env))
  | Sla(x, V(y)) when M.mem y env -> Sla(x, C(M.find y env))
  | Srl(x, V(y)) when M.mem y env -> Srl(x, C(M.find y env))
  | Sra(x, V(y)) when M.mem y env -> Sra(x, C(M.find y env))
  | Load(x, y, V(z)) when M.mem y env -> Load(x, y, C(M.find z env)) (*TODO: 実際には17bitの即値の余裕があるはず*)
  | Store(x, y, V(z)) when M.mem z env -> Store(x, y, C(M.find z env))
  | FLoad(x, y, V(z)) when M.mem y env -> FLoad(x, y, C(M.find z env))
  | FStore(x, y, V(z)) when M.mem z env -> FStore(x, y, C(M.find z env))
  | IfEq(x, V(y), e1, e2) when M.mem y env -> 
      IfEq(x, C(M.find y env), g env e1, g env e2)
  | IfLE(x, V(y), e1, e2) when M.mem y env ->
      IfLE(x, C(M.find y env), g env e1, g env e2)
  | IfGE(x, V(y), e1, e2) when M.mem y env -> 
      IfGE(x, C(M.find y env), g env e1, g env e2)
  | IfEq(x, V(y), e1, e2) when M.mem x env -> 
      IfEq(y, C(M.find x env), g env e1, g env e2)
  | IfLE(x, V(y), e1, e2) when M.mem x env -> 
      IfGE(y, C(M.find x env), g env e1, g env e2)
  | IfGE(x, V(y), e1, e2) when M.mem x env -> 
      IfLE(y, C(M.find x env), g env e1, g env e2)
  | IfEq(x, y', e1, e2) -> IfEq(x, y', g env e1, g env e2)
  | IfLE(x, y', e1, e2) -> IfLE(x, y', g env e1, g env e2)
  | IfGE(x, y', e1, e2) -> IfGE(x, y', g env e1, g env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)
  | e -> e

(* トップレベル関数の 16 bit 即値最適化 *)
let h { name = l; args = xs; fargs = ys; body = e; ret = t } = 
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t }

(* プログラム全体の 16 bit 即値最適化 *)
let f (Prog(data, fundefs, e)) = 
  Prog(data, List.map h fundefs, g M.empty e)
