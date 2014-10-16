type t = { 
  fname : string; (*file name*)
  spos  : int * int;    (*start (line, column)*)
  epos  : int * int;    (*end (line, column)*)
}

val init : t
val convert : int -> int * int
val sprint : t -> string
val add_line : Lexing.lexbuf -> unit
