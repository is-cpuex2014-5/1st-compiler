type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None) (* 新しい型変数を作る *)
		      
let rec sprint  = function 
  | Unit ->  "unit"
  | Bool ->  "bool"
  | Int -> "int"
  | Float -> "float"
  | Fun (l, t) -> "fun " ^
		    (List.fold_left (fun x y -> x ^ (sprint y) ^  "  -> ") "" l) ^
		      sprint  t
  | Tuple l ->  "tuple (" ^
	       (List.fold_left (fun x y -> x ^ (sprint  y) ^ " ") "" l) ^
		 ")"
  | Array t -> "array (" ^ (sprint  t) ^ ")"
  | Var r -> match !r with
	     | None -> "tvar"
	     | Some t -> sprint  t
		      
let rec p oc t = Printf.fprintf oc "%s" (sprint t)
