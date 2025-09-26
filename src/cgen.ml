open Ast

(* let roc = ref (open_out "") *)

let ret_dtype = function
  | Dint -> "w"
  | Dvoid -> ""

let string_of_expr = function
  | Int i -> string_of_int i

let emit_stmt oc = function
  | Ret None -> Printf.fprintf oc "ret"
  | Ret (Some r) -> Printf.fprintf oc "ret %s" (string_of_expr r)

let emit_func oc func =
  Printf.fprintf oc "export function %s $%s() {\n" func.name
    (ret_dtype func.ret_type);
  Printf.fprintf oc "@start\n";
  List.iter
    (fun line ->
      emit_stmt oc line;
      Printf.fprintf oc "\n")
    func.body;
  Printf.fprintf oc "}\n"

let emit_gvar oc gvar =
  let id, dt, e_opt = gvar in
  match e_opt with
  | None -> Printf.fprintf oc "%s $%s\n" id (ret_dtype dt)
  | Some e ->
      Printf.fprintf oc "%s $%s = %s\n" id (ret_dtype dt) (string_of_expr e)

let emitter_driver oc = function
  | Funcs f -> emit_func oc f
  | Global_Vars (id, dt, e_opt) -> emit_gvar oc (id, dt, e_opt)

let emit_program oc (ast : prog) = List.iter (emitter_driver oc) ast
