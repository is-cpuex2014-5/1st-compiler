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
  | Unit -> Printf.fprintf oc "UNIT"
  | Bool -> Printf.fprintf oc "BOOL"
  | Int -> Printf.fprintf oc "INT"
  | Float -> Printf.fprintf oc "FLOAT"
  | Unit -> Printf.fprintf oc "UNIT"
  | Fun (l, t) -> Printf.fprintf oc "FUN ";
		List.iter (fun x -> p oc x; Printf.fprintf oc "  -> ") l;
		p oc t
  | Tuple l -> Printf.fprintf oc "TUPLE ";
	       List.iter (fun x -> p oc x; Printf.fprintf oc " ") l
  | Array t -> Printf.fprintf oc "ARRAY "; p oc t
  | Var r -> match !r with
	     | None -> Printf.fprintf oc "TVar"
	     | Some t -> p oc t
		      

