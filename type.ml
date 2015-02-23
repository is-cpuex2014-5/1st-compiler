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
		      
let rec string_of_type  = function 
  | Unit ->  "unit"
  | Bool ->  "bool"
  | Int -> "int"
  | Float -> "float"
  | Fun (l, t) -> 
     String.concat " -> " (List.map string_of_type  l) ^ 
     " -> " ^ 
     string_of_type  t
  | Tuple l ->  
     "tuple (" ^
     String.concat ", " (List.map string_of_type  l) ^
     ")"
  | Array t -> "array (" ^ (string_of_type  t) ^ ")"
  | Var r -> match !r with
	     | None -> "tvar"
	     | Some t -> string_of_type  t
		      
let rec p oc t = Printf.fprintf oc "%s" (string_of_type t)
