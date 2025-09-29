open Ast
open Symb_table

let check_binop binop el er =
  match (binop, el, er) with
  | (Add | Sub | Mul | Div | Mod), Dint, Dint -> Ok Dint
  | (Add | Sub | Mul | Div | Mod), Dfloat, Dfloat -> Ok Dfloat
  | (Eq | Neq | Les | Grt | Grte | Lese), (Dint | Dfloat), (Dint | Dfloat) ->
      Ok Dint
  | (And | Or), (Dint | Dfloat), (Dint | Dfloat) -> Ok Dint
  | _ -> Error "Invalid type for binary operation."

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
  | Binop (pos, bop, el, er) -> (
      let elt = check_expr el env in
      let ert = check_expr er env in
      match (elt, ert) with
      | Ok tl, Ok tr -> (
          match check_binop bop tl tr with
          | Ok ret_type -> Ok ret_type
          | Error e -> Error (e, pos))
      | Ok _, Error e -> Error e
      | Error e, Ok _ -> Error e
      | Error _, Error _ -> Error ("Invalid type for binary operation.", pos))
  | Uop (pos, up, ex) -> (
      let et = check_expr ex env in
      match et with
      | Ok cet -> (
          match (up, cet) with
          | (Not | Neg | Til), Dint -> Ok Dint
          | (Not | Neg | Til), Dfloat -> Ok Dfloat
          | _ -> Error ("Invalid type for uniary operation.", pos))
      | Error (er, p) -> Error (er, p))
  | Call (pos, fname, exs) ->
      if Funcs.mem env.fns fname then
        let rt, params = Funcs.find env.fns fname in
        let rec check_args_type params exs =
          match (params, exs) with
          | [], [] -> Ok rt
          | (name, dt) :: rparams, exp :: rexs -> (
              let ret_type = check_expr exp env in
              match ret_type with
              | Ok cet ->
                  if cet <> dt then
                    Error ("Different argument type of param " ^ name, pos)
                  else
                    check_args_type rparams rexs
              | Error (er, p) -> Error (er, p))
          | _ -> Error ("Different argument count.", pos)
        in
        check_args_type params exs
      else
        Error ("Unknown function name " ^ fname, pos)
  | Assign (pos, el, er) -> (
      let elt = check_expr el env in
      let ert = check_expr er env in
      match (elt, ert) with
      | Ok tl, Ok tr ->
          if tl <> tr then
            Error ("Different assignment types.", pos)
          else
            Ok tl
      | _ -> Error ("Assignment type check error.", pos))

let rec check_stmts stmt env fdt : (unit, string * pos) result =
  match stmt with
  | Ret (pos, ex) -> (
      match ex with
      | None ->
          if fdt <> Dvoid then
            Error ("Missing return value.", pos)
          else
            Ok ()
      | Some e -> (
          match check_expr e env with
          | Ok dt ->
              if dt <> fdt then
                Error
                  ( "Return statment has different return type then defined in \
                     the function.",
                    pos )
              else
                Ok ()
          | Error (e, p) -> Error (e, p)))
  | Expr (pos, ex) -> (
      match check_expr ex env with
      | Error (e, _) -> Error (e, pos)
      | _ -> Ok ())
  (* missing pushing the declaration in scope and pushing out when the func ends *)
  | Decl (pos, _, decl_type, ex) -> (
      match ex with
      | None -> Ok ()
      | Some e -> (
          match check_expr e env with
          | Ok dt ->
              if dt <> decl_type then
                Error ("Type mismatch in assignment in declaration.", pos)
              else
                Ok ()
          | Error (e, p) -> Error (e, p)))
  | If (pos, ex, stmtl, stmtlo) -> (
      match check_expr ex env with
      | Error (e, p) -> Error (e, p)
      | Ok dt -> (
          if dt = Dvoid then
            Error ("If condition does not take type void", pos)
          else
            let res = check_stmt_list stmtl env fdt in
            match stmtlo with
            | None -> res
            | Some sl -> (
                match res with
                | Error (e, p) -> Error (e, p)
                | Ok () -> check_stmt_list sl env fdt)))
  | While (pos, ex, stmtl) -> (
      match check_expr ex env with
      | Error (e, p) -> Error (e, p)
      | Ok dt -> (
          if dt = Dvoid then
            Error ("While condition does not take type void", pos)
          else
            let res = check_stmt_list stmtl env fdt in
            match res with
            | Error (e, p) -> Error (e, p)
            | Ok () -> Ok ()))
  | Block (pos, stmtl) -> (
      match check_stmt_list stmtl env fdt with
      | Error (e, _) -> Error (e, pos)
      | Ok () -> Ok ())

and check_stmt_list stmtl env fdt =
  match stmtl with
  | [] -> Ok ()
  | stmt :: stmr -> (
      match check_stmts stmt env fdt with
      | Ok () -> check_stmt_list stmr env fdt
      | Error (e, p) -> Error (e, p))

(* let check_prog prog = *)
(*   let env = creat_env () in *)
(*   List.iter (fun decls ->  *)
(*     match decls with  *)
(*     | Global_Vars *)
(*   ) prog *)
