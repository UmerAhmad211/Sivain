%{
open Ast
%}

%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SEMI_C
%token COLON
%token COMMA
%token EQUAL
%token LES GRT
%token ADD MUL SUB DIV MOD
%token NOT TIL
%token LESE GRTE NEQ AND OR
%token FUNC
%token INT_KW VOID_KW FLOAT_KW
%token RET IF ELSE WHILE
%token EOF
%right EQUAL
%left ADD SUB
%left MUL DIV MOD
%nonassoc NOT


%start program 
%type <Ast.prog> program

%%

let program := 
    | decls = list(top_decls); EOF; { decls }

let top_decls := 
    | fn = fun_decl; { Funcs fn }
    | id = ID; COLON; t = dt; option(EQUAL); e = option(expr); SEMI_C; { Global_Vars ( $startpos, id, t, e ) }

let fun_decl := 
    | FUNC; fn_name = ID; LPAREN; p = pparams; RPAREN; rdt = dt; LBRACE; s = list(stmts); RBRACE;
        { { name = ($startpos, fn_name); params = p; ret_type = ($startpos, rdt); body = s } }

let dt == 
    | INT_KW; { Dint }
    | VOID_KW; { Dvoid }
    | FLOAT_KW; { Dfloat }

let pparams := 
    | params = separated_list(COMMA, param); { params }

let param == 
    | id = ID; COLON; pdt = dt; { ( id, pdt, $startpos ) } 

let stmts := 
    | RET; e = option(expr); SEMI_C; { Ret ($startpos, e) } 
    | e = expr; SEMI_C; { Expr ($startpos, e) }
    | id = ID; COLON; t = dt; option(EQUAL); e = option(expr); SEMI_C; { Decl ($startpos, id, t, e) }
    | IF; LPAREN; ce = expr; RPAREN; LBRACE; s = separated_list(SEMI_C, stmts); RBRACE; option(ELSE); option(LBRACE); se = option(separated_list(SEMI_C, stmts)); option(RBRACE);
        { If ($startpos, ce, s, se) }
    | WHILE; LPAREN; e = expr; RPAREN; LBRACE; s = separated_list(SEMI_C, stmts); RBRACE; { While($startpos, e, s) }
    | LBRACE; s = separated_list(SEMI_C, stmts); RBRACE; { Block($startpos, s) }

let expr := 
    | i = INTLIT; { Int ($startpos, i) }
    | f = FLOATLIT; { Float ($startpos, f) }
    | id = ID; { Id_name ($startpos, id) }
    | u = u_op; e = expr; { Uop($startpos, u, e) }
    | el = expr; b = bin_op; er = expr; { Binop($startpos, b, el, er) }
    | id = ID; LPAREN; el = separated_list(COMMA, expr); RPAREN; { Call($startpos, id, el) }
    | lid = expr; EQUAL; rid = expr; { Assign($startpos, lid, rid) }

%inline u_op:
    | NOT { Not }
    | SUB { Neg }
    | TIL { Til }

%inline bin_op:
    | EQUAL EQUAL { Eq }
    | NEQ { Neq }
    | LES { Les }
    | GRT { Grt }
    | LESE { Lese }
    | GRTE { Grte }
    | ADD { Add }
    | MUL { Mul }
    | SUB { Sub }
    | DIV { Div }
    | MOD { Mod }
    | AND { And }
    | OR { Or }
