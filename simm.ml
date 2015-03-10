open Asm

let le16bit x = (-32768 <= x) && (x < 32768)
let le17bit x = (-65536 <= x) && (x < 65536)
let le21bit x = (-1048576 <= x) && (x < 1048576)
let r0 = "$r00"
let f0 = "$f00"

let rec g env fenv = function (* 命令列の即値最適化 *)
  | Ans(exp) -> Ans(g' env fenv exp)
  | Let((x, t), Li(i), e) when le21bit i ->
      let e' = g (M.add x i env) fenv e in
      if List.mem x (fv e') then Let((x, t), Li(i), e') else e'
  | Let((x, t), FLi(l), e) when (let Id.L(l') = l in l') = "fzero" ->
      let e' = g env (S.add x fenv) e in
      if List.mem x (fv e') then Let((x, t), FMov(f0), e') else e'
  | Let(xt, Sll(y, C(i)), e) when M.mem y env -> (* for array access *)
      g env fenv (Let(xt, Li((M.find y env) lsl i), e))
  | Let(xt, exp, e) -> Let(xt, g' env fenv exp, g env fenv e)
and g' env fenv = function (* 各命令の即値最適化 *)
  | Add(x, V(y)) when M.mem y env && (M.find y env) = 0 -> Mov(x)
  | Add(x, V(y)) when M.mem y env && le16bit (M.find y env) -> Add(x, C(M.find y env))
  | Sub(x, V(y)) when M.mem y env && (M.find y env) = 0 -> Mov(x)
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
  | FMov(x) when S.mem x fenv -> FMov(f0)
  | FNeg(x) when S.mem x fenv -> FNeg(f0)
  | FLoad(x, V(y)) when M.mem x env && M.mem y env  && le21bit (M.find x env  + M.find y env) ->
     FLoadi(M.find x env  + M.find y env)
  | FLoad(x, V(y)) when M.mem y env && le17bit (M.find y env) -> FLoad(x, C(M.find y env))
  | FStore(x, y, z') when S.mem x fenv -> g' env fenv (FStore(f0, y, z'))
  | FStore(x, y, V(z)) when M.mem y env && M.mem z env  && le21bit (M.find y env  + M.find z env) ->
     FStorei(x, M.find y env  + M.find z env)
  | FStore(x, y, V(z)) when M.mem z env && le17bit (M.find z env) -> FStore(x, y, C(M.find z env))
  | IfEq(x, y, e1, e2) when M.mem x env  && (M.find x env) = 0 -> (*optimize if one of the reg is zero*)
     g' env fenv (IfEq(r0, y, e1, e2))
  | IfEq(x, y, e1, e2) when M.mem y env  && (M.find y env) = 0 ->
     g' env fenv (IfEq(x, r0, e1, e2))
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env fenv e1, g env fenv e2)
  | IfLT(x, y, e1, e2) when M.mem x env  && (M.find x env) = 0 -> 
     g' env fenv (IfLT(r0, y, e1, e2) )
  | IfLT(x, y, e1, e2) when M.mem y env  && (M.find y env) = 0 ->
     g' env fenv (IfLT(x, r0, e1, e2))
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g env fenv e1, g env fenv e2)
  | IfFEq(x, y, e1, e2) when S.mem x fenv -> g' env fenv (IfFEq(f0, y, e1, e2))
  | IfFEq(x, y, e1, e2) when S.mem y fenv -> g' env fenv (IfFEq(x, f0, e1, e2))
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env fenv e1, g env fenv e2)
  | IfFLT(x, y, e1, e2) when S.mem x fenv -> g' env fenv (IfFLT(f0, y, e1, e2))
  | IfFLT(x, y, e1, e2) when S.mem y fenv -> g' env fenv (IfFLT(x, f0, e1, e2))
  | IfFLT(x, y, e1, e2) -> IfFLT(x, y, g env fenv e1, g env fenv e2)
  | e -> e

(* トップレベル関数の即値最適化 *)
let h { name = l; args = xs; fargs = ys; body = e; ret = t } = 
  { name = l; args = xs; fargs = ys; body = g M.empty S.empty e; ret = t }

(* プログラム全体の即値最適化 *)
let f (Prog(data, fundefs, e)) = 
  Prog(data, List.map h fundefs, g M.empty S.empty e)


