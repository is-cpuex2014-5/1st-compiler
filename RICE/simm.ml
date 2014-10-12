open Asm

let le16bit x = (-32768 <= x) && (x < 32768)
let le17bit x = (-65536 <= x) && (x < 65536)
let le21bit x = (-1048576 <= x) && (x < 1048576)

let rec g env = function (* 命令列の即値最適化 *)
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t), Li(i), e) when le21bit i ->
      let e' = g (M.add x i env) e in
      if List.mem x (fv e') then Let((x, t), Li(i), e') else e'
  | Let(xt, Sll(y, C(i)), e) when M.mem y env -> (* for array access *)
      g env (Let(xt, Li((M.find y env) lsl i), e))
  | Let(xt, exp, e) -> Let(xt, g' env exp, g env e)
and g' env = function (* 各命令の即値最適化 *)
  | Add(x, V(y)) when M.mem y env && le16bit (M.find y env) -> Add(x, C(M.find y env))
  | Sub(x, V(y)) when M.mem y env && le16bit (M.find y env) -> Sub(x, C(M.find y env))
  | Sll(x, V(y)) when M.mem y env -> Sll(x, C(M.find y env))
  | Sla(x, V(y)) when M.mem y env -> Sla(x, C(M.find y env))
  | Srl(x, V(y)) when M.mem y env -> Srl(x, C(M.find y env))
  | Sra(x, V(y)) when M.mem y env -> Sra(x, C(M.find y env))
  | Load(x, V(y)) when M.mem x env && M.mem y env  && le21bit (M.find x env  + M.find y env) ->
     Loadi(M.find x env  + M.find y env)
  | Load(x, V(y)) when M.mem y env && le17bit (M.find y env) -> Load(x, C(M.find y env))
  | Store(x, y, V(z)) when M.mem y env && M.mem z env  && le21bit (M.find y env  + M.find z env) ->
     Storei(x, M.find y env  + M.find z env)
  | Store(x, y, V(z)) when M.mem z env && le17bit (M.find z env) -> Store(x, y, C(M.find z env))
  | FLoad(x, V(y)) when M.mem x env && M.mem y env  && le21bit (M.find x env  + M.find y env) ->
     FLoadi(M.find x env  + M.find y env)
  | FLoad(x, V(y)) when M.mem y env && le17bit (M.find y env) -> FLoad(x, C(M.find y env))
  | FStore(x, y, V(z)) when M.mem y env && M.mem z env  && le21bit (M.find y env  + M.find z env) ->
     FStorei(x, M.find y env  + M.find z env)
  | FStore(x, y, V(z)) when M.mem z env && le17bit (M.find z env) -> FStore(x, y, C(M.find z env))
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

(* トップレベル関数の即値最適化 *)
let h { name = l; args = xs; fargs = ys; body = e; ret = t } = 
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t }

(* プログラム全体の即値最適化 *)
let f (Prog(data, fundefs, e)) = 
  Prog(data, List.map h fundefs, g M.empty e)


