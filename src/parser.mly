%{
    open Ast
%}

%token <int> INTLIT
%token <string> ID
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SEMI_C
%token INT_KW
%token FUNC
%token RET
%token EOF

%start program 
%type <Ast.prog> program

%%

program: 
    | funcs EOF { List.rev $1 }

funcs:
    | func { [$1] }
    | func funcs { $1 :: $2 }

func: 
    | FUNC ID LPAREN RPAREN dtype LBRACE lines RBRACE
        { { name = $2; params = []; ret_type = $5; body = $7 } }

dtype:
    | INT_KW { Dint }

lines:
    | line { [$1] }
    | line lines { $1 :: $2 }

line:
    | RET exp SEMI_C { Ret $2 }

exp: 
    | INTLIT { Int $1 }
