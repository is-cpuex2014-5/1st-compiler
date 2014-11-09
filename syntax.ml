type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Itof of t
  | Ftoi of t
  | Eq of t * t
  | LT of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
  | Pos of Pos.t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let getexp = function  (*Pos(p, Pos(p',e)) won't appear*)
    Pos(_, e) ->   e
  | e -> e

let rec indent oc =function
  | 0 -> ()
  | n -> Printf.fprintf oc "  "; indent oc (n-1)

let pname oc fdef =  let (s, t) = fdef.name in
		     Printf.fprintf oc "NAME %s : " s; Type.p oc t; Printf.fprintf oc " \n"
let pargs oc fdef =  Printf.fprintf oc "ARGS "; 
		     List.iter (fun (x,y) -> Printf.fprintf oc " %s : " x; Type.p oc y) fdef.args; Printf.fprintf oc " \n"

let p oc e = 
  let rec f n e' = 
    let rec g n = function
      | Unit -> Printf.fprintf oc "UNIT\n"
      | Bool true -> Printf.fprintf oc "BOOL true\n" 
      | Bool false -> Printf.fprintf oc "BOOL false\n" 
      | Int i -> Printf.fprintf oc "INT %d\n" i
      | Float f -> Printf.fprintf oc "FLOAT %f\n" f
      | Not e -> Printf.fprintf oc "NOT\n"; f (n+1) e
      | Neg e -> Printf.fprintf oc "NEG\n"; f (n+1) e
      | Add (e1, e2) -> Printf.fprintf oc "ADD\n"; f (n+1) e1; f (n+1) e2
      | Sub (e1, e2) -> Printf.fprintf oc "SUB\n"; f (n+1) e1; f (n+1) e2
      | FNeg e -> Printf.fprintf oc "FNEG\n"; f (n+1) e
      | FAdd (e1, e2) -> Printf.fprintf oc "FADD\n"; f (n+1) e1; f (n+1) e2
      | FSub (e1, e2) -> Printf.fprintf oc "FSUB\n"; f (n+1) e1; f (n+1) e2
      | FMul (e1, e2) -> Printf.fprintf oc "FMUL\n"; f (n+1) e1; f (n+1) e2
      | FDiv (e1, e2) -> Printf.fprintf oc "FDIV\n"; f (n+1) e1; f (n+1) e2
      | Itof (e) -> Printf.fprintf oc "ITOF\n"; f (n+1) e;
      | Ftoi (e) -> Printf.fprintf oc "FTOI\n"; f (n+1) e;
      | Eq (e1, e2) -> Printf.fprintf oc "EQ\n"; f (n+1) e1; f (n+1) e2
      | LT (e1, e2) -> Printf.fprintf oc "LT\n"; f (n+1) e1; f (n+1) e2
      | If (e1, e2, e3) ->  Printf.fprintf oc "IF\n"; f (n+1) e1; f (n+1) e2; f (n+1) e3
      | Let ((s, t), e1, e2) ->  Printf.fprintf oc "LET %s : " s; Type.p oc t; Printf.fprintf oc " \n";
				 f (n+1) e1; f (n+1) e2;
      | Var s -> Printf.fprintf oc "VAR %s\n" s
      | LetRec (fdef, e1) -> Printf.fprintf oc "LETREC\n";
			     indent oc (n+1); pname oc fdef;
			     indent oc (n+1); pargs oc fdef;
			     indent oc (n+1); Printf.fprintf oc "BODY\n"; f (n+2) fdef.body;
			     f (n+1) e1
      | App (e1, l) -> Printf.fprintf oc "APP\n"; f (n+1) e1; List.iter (f (n+1)) l 
      | Tuple l ->  Printf.fprintf oc "TUPLE\n"; List.iter (f (n+1)) l       
      | LetTuple (l, e1, e2) -> Printf.fprintf oc "LETTUPLE ";List.iter (fun (x,y) -> Printf.fprintf oc " %s : " x; Type.p oc y) l; Printf.fprintf oc " \n";
				f (n+1) e1; f (n+1) e2
      | Array (e1, e2) -> Printf.fprintf oc "ARRAY\n"; f (n+1) e1; f (n+1) e2
      | Get (e1, e2) -> Printf.fprintf oc "GET\n"; f (n+1) e1; f (n+1) e2
      | Put (e1, e2, e3) ->  Printf.fprintf oc "PUT\n"; f (n+1) e1; f (n+1) e2; f (n+1) e3
      | Pos (_, e1) -> g n e1
    in 
    indent oc n; g n e'
  in f 0 e; e
