open Ast
open Symb_table

let rec check_expr e env : (dtype, string * pos) result =
  match e with
  | Int _ -> Ok Dint
  | Float _ -> Ok Dfloat
  | Id_name (pos, name) ->
      if Vars.mem env.vars name then
        match Vars.find env.vars name with
        | Error e -> Error (e, pos)
        | Ok dt -> Ok dt
      else
        Error ("Undeclared variable: " ^ name, pos)
  | _ -> failwith "Not supported"

(* let check_prog prog = *)
(*   let env = creat_env () in *)
(*   List.iter (fun decls ->  *)
(*     match decls with  *)
(*     | Global_Vars *)
(*   ) prog *)
