open KNormal
(* tuple expand for functions*)

(* env: Id.t -> type typev list *)
(* env fname -> types of args*)

let fvf x e =
  let rec go = function
    | IfEq (_, _, e1, e2) | IfLT (_, _, e1, e2)
    | Let (_, e1, e2) | LetRec ({ body = e1 }, e2) -> go e1 || go e2
    | LetTuple (_, _, e) -> go e
    | Var y | Put (_, _, y) -> x = y
    | App (_, ys) | ExtFunApp (_, ys) | Tuple ys -> List.exists (fun y -> x = y) ys
    | _ -> false
  in
    go e



let rec g env = function
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
     if fvf x e1 || fvf x e2 then
       LetRec({ name = (x, t); args = yts; body = g env e1 }, g env e2) 
     else (*main part*)
       let ts = List.map snd yts in
       let env' = M.add x ts env in
       let (yts', e1') = (*new args and body*)
         List.fold_right
            (fun (y, t) (yts, e1) ->
             match t with
             | Type.Tuple ts ->
                Printf.fprintf stderr "Tuple in the argument is expanded"; 
                let zts = List.map (fun t -> (Id.gentmp t, t)) ts in
                let zs = List.map fst zts in
                (zts @ yts, Let ((y, t), Tuple zs, e1))
             | _ -> ((y, t) :: yts, e1))
            yts ([], e1)
       in
       let t' =  (*new type*)
	 (match t with 
	  | Type.Fun (_, ret) -> Type.Fun (List.map snd yts, ret) 
	  | _ -> assert false) in 
       LetRec( { name = (x, t'); args = yts'; body = g env' e1' }, g env' e2)
  | App (f, xs) when M.mem f env -> (*if the definition of f has been changed*)
     let id = (fun x -> x) in
     let (k, xs') = (*k is a continuation*)
       List.fold_right2
         (fun x t (k, xs) ->
          match t with
          | Type.Tuple(ts) -> 
             let yts = List.map (fun t -> (Id.gentmp t, t)) ts in
             let ys = List.map fst yts in
             ((fun e -> LetTuple (yts, x, k e)), ys @ xs)
          | _ -> (k, x :: xs))
         xs (M.find f env) (id, []) in
     k (App (f, xs'))
  | IfEq (x, y, e1, e2) -> IfEq (x, y, g env e1, g env e2)
  | IfLT (x, y, e1, e2) -> IfLT (x, y, g env e1, g env e2)
  | Let (xt, e1, e2) -> Let (xt, g env e1, g env e2)
  | LetTuple (xts, y, e) -> LetTuple (xts, y, g env e)
  | e -> e


let f e = g M.empty e
