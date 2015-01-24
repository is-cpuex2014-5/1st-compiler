open Unix
let limit = ref 1000
let foutname = ref ""
let temp = "a"
let buffer = String.create 1024

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f ((*TupleArgExpand.f *)(Inline.f (Assoc.f (Cse.f (Beta.f e)))))) in
  if e = e' then e else
  iter (n - 1) e'

let lexbuf outchan debugchan l = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  Emit.f outchan
    (RegAlloc.f
       (Simm.f
	  (Asm.p' debugchan
	  (Virtual.f
	     (Closure.f
		(KNormal.p debugchan
		(iter !limit
		   ((*KNormal.p debugchan*)
		   (Alpha.f
		   ((*KNormal.p debugchan*)
		      (KNormal.f
		      (Syntax.p debugchan
			 (Typing.f
			    (Parser.exp Lexer.token l))))))))))))))

let string s = lexbuf Pervasives.stdout Pervasives.stderr (Lexing.from_string s) (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (!foutname ^ ".s") in
  let debugchan = open_out (!foutname ^ "_dbg.txt") in 
  try
    lexbuf outchan debugchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
    close_out debugchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let merge f = 
 let fd_in = openfile (f ^ ".ml") [O_RDONLY] 0 in
 let fd_out = openfile (temp ^ ".ml") [O_WRONLY; O_CREAT; O_APPEND] 0o666 in
 let rec copy_loop () = match read fd_in buffer 0 1024 with
   |  0 -> ()
   | r -> ignore (write fd_out buffer 0 r); copy_loop ()
 in
 copy_loop ();
 close fd_in;
 close fd_out
  

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
     ("-finv", Arg.Unit(fun () -> Asm.invflag := true), "use finv instruction");
     ("-fsqrt", Arg.Unit(fun () -> Asm.sqrtflag := true), "use fsqrt instruction")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] [-finv] [-fsqrt] ...filenames without \".ml\"..." Sys.argv.(0));
  let fd_out = openfile (temp ^ ".ml") [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  close fd_out;
  foutname := List.hd (List.rev !files);
  List.iter
    (fun f -> ignore (merge f))
    !files;
    ignore (file  temp)
