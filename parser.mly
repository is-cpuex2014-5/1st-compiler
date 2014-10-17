%{
(* parserが利用する変数、関数、型などの定義 *)
open Syntax
let addtyp x = (x, Type.gentyp ())
let addpos e =  (*TODO: add file name*)
  let snum = Parsing.symbol_start () in
  let enum = Parsing.symbol_end () in
  let spos = Pos.convert snum in
  let epos = Pos.convert enum in 
  let p = {
    Pos.fname = "";
    Pos.spos = spos;
    Pos.epos = epos;
  } in
  Pos (p, e)
  
%}

/* 字句を表すデータ型の定義 (caml2html: parser_token) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST 
%token SLASH
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF

/* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) */
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST SLASH AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT

/* 開始記号の定義 */
%type <Syntax.t> exp
%start exp

%%

simple_exp: /* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit }
| BOOL
    { Bool($1) }
| INT
    { Int($1) }
| FLOAT
    { Float($1) }
| IDENT
    { Var($1) }
| simple_exp DOT LPAREN exp RPAREN
    { Get($1, $4) }

exp: /* 一般の式 (caml2html: parser_exp) */
| simple_exp
    { addpos $1 }
| NOT exp
    %prec prec_app
    { addpos (Not($2)) }
| MINUS exp
    %prec prec_unary_minus
    { addpos (match (getexp $2) with
	      | Float(f) -> (Float(-.f)) (* -1.23などは型エラーではないので別扱い *)
	      | e -> (Neg(e))) }
| exp PLUS exp /* 足し算を構文解析するルール (caml2html: parser_add) */
    { addpos (Add($1, $3)) }
| exp MINUS exp
    { addpos (Sub($1, $3)) }
| exp EQUAL exp
    { addpos (Eq($1, $3)) }
| exp LESS_GREATER exp
    { addpos (Not(Eq($1, $3))) }
| exp LESS exp
    { addpos (Not(LE($3, $1))) }
| exp GREATER exp
    { addpos (Not(LE($1, $3))) }
| exp LESS_EQUAL exp
    { addpos (LE($1, $3)) }
| exp GREATER_EQUAL exp
    { addpos (LE($3, $1)) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { addpos (If($2, $4, $6)) }
| MINUS_DOT exp
    %prec prec_unary_minus
    { addpos (FNeg($2)) }
| exp PLUS_DOT exp
    { addpos (FAdd($1, $3)) }
| exp MINUS_DOT exp
    { addpos (FSub($1, $3)) }
| exp AST exp
    { addpos (App (Var "mul", [$1; $3])) }
| exp SLASH exp
    { addpos (App (Var "div", [$1; $3])) }
| exp AST_DOT exp
    { addpos (FMul($1, $3)) }
| exp SLASH_DOT exp
    { addpos (FDiv($1, $3)) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { addpos (Let(addtyp $2, $4, $6)) }
| LET REC fundef IN exp
    %prec prec_let
    { addpos (LetRec($3, $5)) }
| exp actual_args
    %prec prec_app
    { addpos (App($1, $2)) }
| elems
    { addpos (Tuple($1)) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { addpos (LetTuple($3, $6, $8)) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { addpos (Put($1, $4, $7)) }
| exp SEMICOLON exp
    { addpos (Let((Id.gentmp Type.Unit, Type.Unit), $1, $3)) }
| exp SEMICOLON 
    { addpos (Let((Id.gentmp Type.Unit, Type.Unit), $1, Unit)) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { addpos (Array($2, $3)) }
| error
    { let (line, column) = Pos.convert (Parsing.symbol_start ())
      in
      failwith
	
	( Printf.sprintf "line %d, column %d : Parse error\n" line column)
    }

fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { $1 @ [addtyp $3] }
| IDENT COMMA IDENT
    { [addtyp $1; addtyp $3] }
