open KNormal

module KM = (*Map : KNormal.t -> 'a *)
  Map.Make
    (struct
      type t = KNormal.t
      (*Enable to get GET_ by taking the minimum element*)
      let order = function Get _ -> 0 | _ -> 1 
      let compare x y = compare (order x, x) (order y, y) 	       
    end)

let rec has_side_effect = function (*Only an approximation*)
  | Let (_, e1, e2) | IfEq (_, _, e1, e2) | IfLE (_, _, e1, e2) -> has_side_effect e1 || has_side_effect e2
  | LetRec (_, e) | LetTuple (_, _, e) -> has_side_effect e
  | App _ | Put _ | ExtFunApp _ -> true
  | _ -> false

let rec remove_get = function
  | env when KM.is_empty env -> env
  | env -> match KM.min_binding env with
      | (Get _ as e, _) -> remove_get (KM.remove e env)
      | _ -> env

let add_get env = function 
  | Put(x, y, z) -> KM.add (Get (x, y)) z env
  | _ -> env

let replace e env  = try Var(KM.find e env) with
		       Not_found -> e

(*main routine*)
let rec g env = function 
  | Let((x, t), e1, e2) -> 
     let e1' = g env e1 in
     let env' = if has_side_effect e1'
		then remove_get env 
		else KM.add e1' x  env in
     let env'' = add_get env' e1' in
     Let((x, t), e1', g env'' e2)
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2) -> (*elimination won't be done to avoid making extra closures*)
      LetRec ({ name = (x,t); args = yts; body =  g KM.empty e1 }, g env e2)
  | LetTuple (xts, y, e) -> (*TODO:*)
     LetTuple (xts, y, g env e)
  | IfEq (x, y, e1, e2) ->
     let e' = IfEq (x, y, g env e1, g env e2) in
     replace e' env
  | IfLE (x, y, e1, e2) ->
     let e' = IfLE (x, y, g env e1, g env e2) in
     replace e' env
							    
  | e -> replace e env

let f e = g KM.empty e
