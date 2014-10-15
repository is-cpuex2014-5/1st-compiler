type t = { 
  fname : string; (*file name*)
  slnum : int;    (*start of the line*)
  scnum : int;    (*start of the column*)
  elnum : int;    (*end of the line*)
  ecnum : int;    (*end of the column*)
}

let line_buf = ref [0] (*the number of the characters in the end of the line will be added*)

let num2pos n =
    let xs = List.filter (fun x -> x <= n) !line_buf in
      (List.length xs, n - List.hd xs + 1)

let sprint  = function 
    p when p.slnum == p.elnum -> 
	   Printf.sprintf "file: %s, line: %d, column: %d to %d\n" p.fname p.slnum p.scnum p.ecnum
  | p -> 
     Printf.sprintf "file: %s, line: %d to %d, column: %d to %d\n" p.fname p.slnum p.elnum p.scnum p.ecnum
		     
let add_line lexbuf = line_buf := Lexing.lexeme_end lexbuf :: !line_buf


