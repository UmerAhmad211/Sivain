{
open Lexing
open Parser

exception Syntax_Error of string

let next_line lexbuf = 
    let pos = lexbuf.lex_curr_p in 
    lexbuf.lex_curr_p <- {
        pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
    }
}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z' ]
let int = '-'? digit+
let ident = ( alpha ) ( alpha|digit|'_' )*
let whitespace = [ ' ' '\t' ]+
let newline = '\r' | '\n' | "\r\n"
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

rule read_token = parse
    | whitespace    { read_token lexbuf }
    | "(*"          { read_comment lexbuf }
    | '('           { LPAREN }
    | ')'           { RPAREN }
    | '{'           { LBRACE }
    | '}'           { RBRACE }
    | ';'           { SEMI_C }
    | ':'           { COLON }
    | ','           { COMMA }
    | '='           { EQUAL }
    | '<'           { LES }
    | '>'           { GRT }
    | '+'           { ADD }
    | '*'           { MUL }
    | '-'           { SUB }
    | '/'           { DIV }
    | '%'           { MOD }
    | '!'           { NOT }
    | '~'           { TIL }
    | "<="          { LESE }
    | ">="          { GRTE }
    | "!="          { NEQ }
    | "&&"          { AND }
    | "||"          { OR }
    | "fun"         { FUNC }
    | "int"         { INT_KW }
    | "void"        { VOID_KW }
    | "float"       { FLOAT_KW }
    | "ret"         { RET }
    | "if"          { IF }
    | "else"        { ELSE }
    | "while"       { WHILE }
    | int           { INTLIT (int_of_string (Lexing.lexeme lexbuf)) }
    | float         { FLOATLIT (float_of_string (Lexing.lexeme lexbuf)) }
    | ident         { ID (Lexing.lexeme lexbuf) }
    | newline       { next_line lexbuf; read_token lexbuf }
    | eof           { EOF }
    | _             { raise (Syntax_Error ("Sivain: Error: Illegal character: " ^ Lexing.lexeme lexbuf)) }


and read_comment = parse
    | "*)"          { read_token lexbuf }
    | newline       { next_line lexbuf; read_comment lexbuf }
    | eof           { raise (Syntax_Error ("Sivain: Error: Unexpected EOF: Comment terminate error.")) }
    | _             { read_comment lexbuf }
