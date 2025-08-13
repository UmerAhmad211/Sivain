open Ast

let ret_dtype = function
  | Dint -> "w"
  | Dvoid -> ""

let rec emit_exp oc = function
  | Int n -> Printf.fprintf oc "%d" n
  | Ret r ->
      Printf.fprintf oc "ret ";
      emit_exp oc r

let emit_func oc func =
  Printf.fprintf oc "export function %s $%s() {\n" (ret_dtype func.ret_type)
    func.name;
  Printf.fprintf oc "@start\n";
  List.iter
    (fun line ->
      emit_exp oc line;
      Printf.fprintf oc "\n")
    func.body;
  Printf.fprintf oc "}\n"

let emit_program oc (p : prog) = List.iter (emit_func oc) p
