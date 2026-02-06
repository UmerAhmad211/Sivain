open Ast

type parsed_expr = { e : expr; ty : dtype }

type parsed_stmt =
  | PRet of pos * parsed_expr option
  | PExpr of pos * parsed_expr
  | PDecl of pos * string * dtype * parsed_expr option
  | PIf of pos * parsed_expr * parsed_stmt list * parsed_stmt list option
  | PWhile of pos * parsed_expr * parsed_stmt list
  | PBlock of pos * parsed_stmt list

type parsed_func = {
  pname : pos * string;
  pparams : (string * dtype * pos) list;
  pret_type : pos * dtype;
  pbody : parsed_stmt list;
}

type parsed_top_level_decls =
  | PFuncs of parsed_func
  | PGlobal_Vars of pos * string * dtype * parsed_expr option

type parsed_prog = parsed_top_level_decls list

let indent n = String.make (2 * n) ' '
let opt f = function None -> "None" | Some x -> f x

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

let string_of_dtype = function
  | Dint -> "int"
  | Dfloat -> "float"
  | Dvoid -> "void"

let rec string_of_expr = function
  | Int (_, i) -> string_of_int i
  | Float (_, f) -> string_of_float f
  | Id_name (_, s) -> s
  | Binop (_, op, e1, e2) ->
      Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_binop op)
        (string_of_expr e2)
  | Uop (_, op, e) ->
      Printf.sprintf "(%s%s)" (string_of_uop op) (string_of_expr e)
  | Call (_, name, args) ->
      Printf.sprintf "%s(%s)" name
        (String.concat ", " (List.map string_of_expr args))
  | Assign (_, lhs, rhs) ->
      Printf.sprintf "(%s = %s)" (string_of_expr lhs) (string_of_expr rhs)

let string_of_parsed_expr pe =
  Printf.sprintf "%s : %s" (string_of_expr pe.e) (string_of_dtype pe.ty)

let rec string_of_parsed_stmt lvl = function
  | PRet (_, None) -> indent lvl ^ "return;"
  | PRet (_, Some pe) -> indent lvl ^ "ret " ^ string_of_parsed_expr pe ^ ";"
  | PExpr (_, pe) -> indent lvl ^ string_of_parsed_expr pe ^ ";"
  | PDecl (_, name, ty, init) ->
      indent lvl
      ^ Printf.sprintf "%s : %s%s;" name (string_of_dtype ty)
          (match init with
          | None -> ""
          | Some pe -> " = " ^ string_of_parsed_expr pe)
  | PIf (_, cond, then_blk, else_blk) ->
      let then_part =
        String.concat "\n" (List.map (string_of_parsed_stmt (lvl + 1)) then_blk)
      in
      let else_part =
        match else_blk with
        | None -> ""
        | Some blk ->
            "\n" ^ indent lvl ^ "else {\n"
            ^ String.concat "\n"
                (List.map (string_of_parsed_stmt (lvl + 1)) blk)
            ^ "\n" ^ indent lvl ^ "}"
      in
      indent lvl ^ "if (" ^ string_of_parsed_expr cond ^ ") {\n" ^ then_part
      ^ "\n" ^ indent lvl ^ "}" ^ else_part
  | PWhile (_, cond, body) ->
      indent lvl ^ "while (" ^ string_of_parsed_expr cond ^ ") {\n"
      ^ String.concat "\n" (List.map (string_of_parsed_stmt (lvl + 1)) body)
      ^ "\n" ^ indent lvl ^ "}"
  | PBlock (_, stmts) ->
      indent lvl ^ "{\n"
      ^ String.concat "\n" (List.map (string_of_parsed_stmt (lvl + 1)) stmts)
      ^ "\n" ^ indent lvl ^ "}"

let string_of_param (name, ty, _) =
  Printf.sprintf "%s : %s" name (string_of_dtype ty)

let string_of_parsed_func f =
  let params = String.concat ", " (List.map string_of_param f.pparams) in
  let body = String.concat "\n" (List.map (string_of_parsed_stmt 1) f.pbody) in
  let _, name = f.pname in
  let _, ret_ty = f.pret_type in
  Printf.sprintf "func %s(%s) : %s {\n%s\n}" name params
    (string_of_dtype ret_ty) body

let string_of_top_level = function
  | PFuncs f -> string_of_parsed_func f
  | PGlobal_Vars (_, name, ty, init) ->
      Printf.sprintf "global %s : %s%s;" name (string_of_dtype ty)
        (match init with
        | None -> ""
        | Some pe -> " = " ^ string_of_parsed_expr pe)

let string_of_parsed_prog prog =
  String.concat "\n\n" (List.map string_of_top_level prog)

let print_parsed_prog prog = print_endline (string_of_parsed_prog prog)
