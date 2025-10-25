open Ast

let ret_dtype = function
  | Dint -> "w"
  | Dvoid -> ""
  | Dfloat -> "s"

let string_of_expr = function
  | Int (_, i) -> string_of_int i
  | Float (_, f) -> string_of_float f
  | _ -> ""

let emit_stmt oc stmt =
  match stmt with
  | Ret (_, ex) -> (
      match ex with
      | None -> Printf.fprintf oc "ret"
      | Some e -> Printf.fprintf oc "ret %s" (string_of_expr e))
  | _ -> ()

let emit_func oc func =
  let _, fname = func.name in
  let _, rt = func.ret_type in
  Printf.fprintf oc "export function %s $%s() {\n" (ret_dtype rt) fname;
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
  | Global_Vars (_, id, dt, e_opt) -> emit_gvar oc (id, dt, e_opt)

let emit_program oc (ast : prog) = List.iter (emitter_driver oc) ast
