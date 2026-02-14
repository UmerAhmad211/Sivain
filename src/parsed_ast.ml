open Ast
open Symb_table

type scope = Vars.t

type parsed_stmt =
  | PRet of expr option
  | PExpr of expr
  | PDecl of string * dtype * expr option
  | PIf of expr * block_scope * block_scope option
  | PWhile of expr * block_scope
  | PBlock of block_scope

and block_scope = { bscope : scope; stmts : parsed_stmt list }

type parsed_func = {
  pname : string;
  pparams : (string * dtype) list;
  pret_type : dtype;
  pbody : block_scope;
}

type parsed_top_level_decls =
  | PFuncs of parsed_func
  | PGlobal_Vars of string * dtype * expr option

type parsed_prog = parsed_top_level_decls list

let string_of_binop = function
  | Eq -> "=="
  | Neq -> "!="
  | Les -> "<"
  | Grt -> ">"
  | Lese -> "<="
  | Grte -> ">="
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function Not -> "!" | Neg -> "-" | Til -> "~"

let rec string_of_expr = function
  | Int (_, i) -> string_of_int i
  | Float (_, f) -> string_of_float f
  | Id_name (_, id) -> id
  | Uop (_, op, e) ->
      Printf.sprintf "(%s %s)" (string_of_uop op) (string_of_expr e)
  | Binop (_, op, l, r) ->
      Printf.sprintf "(%s %s %s)" (string_of_expr l) (string_of_binop op)
        (string_of_expr r)
  | Call (_, name, args) ->
      Printf.sprintf "%s(%s)" name
        (String.concat ", " (List.map string_of_expr args))
  | Assign (_, l, r) ->
      Printf.sprintf "(%s = %s)" (string_of_expr l) (string_of_expr r)

let rec string_of_stmt = function
  | PRet None -> "return;"
  | PRet (Some e) -> "return " ^ string_of_expr e ^ ";"
  | PExpr e -> string_of_expr e ^ ";"
  | PDecl (name, dt, None) ->
      Printf.sprintf "%s : %s;" name (string_of_dtype dt)
  | PDecl (name, dt, Some e) ->
      Printf.sprintf "%s : %s = %s;" name (string_of_dtype dt)
        (string_of_expr e)
  | PIf (cond, then_s, None) ->
      Printf.sprintf "if (%s) %s" (string_of_expr cond) (string_of_block then_s)
  | PIf (cond, then_s, Some else_s) ->
      Printf.sprintf "if (%s) %s else %s" (string_of_expr cond)
        (string_of_block then_s) (string_of_block else_s)
  | PWhile (cond, body) ->
      Printf.sprintf "while (%s) %s" (string_of_expr cond)
        (string_of_block body)
  | PBlock stmts -> string_of_block stmts

and string_of_block stmts =
  "{\n" ^ String.concat "\n" (List.map string_of_stmt stmts.stmts) ^ "\n}"

and string_of_dtype = function
  | Dint -> "int"
  | Dfloat -> "float"
  | Dvoid -> "void"

let string_of_param (name, ty) =
  Printf.sprintf "%s : %s" name (string_of_dtype ty)

let string_of_func (f : parsed_func) =
  Printf.sprintf "func %s(%s) : %s %s" f.pname
    (String.concat ", " (List.map string_of_param f.pparams))
    (string_of_dtype f.pret_type)
    (string_of_block f.pbody)

let string_of_top = function
  | PFuncs f -> string_of_func f
  | PGlobal_Vars (name, dt, None) ->
      Printf.sprintf "global %s : %s;" name (string_of_dtype dt)
  | PGlobal_Vars (name, dt, Some e) ->
      Printf.sprintf "global %s : %s = %s;" name (string_of_dtype dt)
        (string_of_expr e)

let print_parsed_ast p =
  print_endline (String.concat "\n\n" (List.map string_of_top p))
