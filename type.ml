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



let rec p oc = function 
  | Unit -> Printf.fprintf oc "unit"
  | Bool -> Printf.fprintf oc "bool"
  | Int -> Printf.fprintf oc "int"
  | Float -> Printf.fprintf oc "float"
  | Fun (l, t) -> Printf.fprintf oc "fun ";
		List.iter (fun x -> p oc x; Printf.fprintf oc "  -> ") l;
		p oc t
  | Tuple l -> Printf.fprintf oc "tuple ";
	       List.iter (fun x -> p oc x; Printf.fprintf oc " ") l
  | Array t -> Printf.fprintf oc "array "; p oc t
  | Var r -> match !r with
	     | None -> Printf.fprintf oc "tvar"
	     | Some t -> p oc t
		      
let rec sprint  = function 
  | Unit ->  "unit"
  | Bool ->  "bool"
  | Int -> "int"
  | Float -> "float"
  | Fun (l, t) -> "fun " ^
		    (List.fold_left (fun x y -> x ^ (sprint y) ^  "  -> ") "" l) ^
		      sprint  t
  | Tuple l ->  "tuple " ^
	       (List.fold_left (fun x y -> x ^ (sprint  y) ^ " ") "" l)
  | Array t -> "array " ^ (sprint  t)
  | Var r -> match !r with
	     | None -> "tvar"
	     | Some t -> sprint  t
		      
