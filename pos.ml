type t = { 
  fname : string; (*file name*)
  spos  : int * int;    (*start (line, column)*)
  epos  : int * int;    (*end (line, column)*)
}

let init = {fname = ""; spos = (0,0); epos = (0,0)}
let line_buf = ref [0] (*the number of the characters in the end of the line will be added*)

(*int -> (int, int)*)
let convert n =
    let xs = List.filter (fun x -> x <= n) !line_buf in
      (List.length xs, n - List.hd xs + 1)

let sprint  = function 
    p when (fst p.spos) == (fst p.epos) -> 
	   Printf.sprintf "file: %s, line: %d, column: %d to %d" p.fname (fst p.spos) (snd p.spos) (snd p.epos)
  | p -> 
     Printf.sprintf "file: %s, line: %d to %d, column: %d to %d" p.fname (fst p.spos) (fst p.epos) (snd p.spos) (snd p.epos)
		     
let add_line lexbuf = line_buf := Lexing.lexeme_end lexbuf :: !line_buf


