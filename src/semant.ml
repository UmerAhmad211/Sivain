open Ast
open Symb_table

let check_binop binop el er =
  match (binop, el, er) with
  | (Add | Sub | Mul | Div | Mod), Dint, Dint -> Ok Dint
  | (Add | Sub | Mul | Div | Mod), Dfloat, Dfloat -> Ok Dfloat
  | (Eq | Neq | Les | Grt | Grte | Lese), (Dint | Dfloat), (Dint | Dfloat) ->
      Ok Dint
  | (And | Or), (Dint | Dfloat), (Dint | Dfloat) -> Ok Dint
  | _ -> Error "Invalid type for binary operation"

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
      | Error _, Error _ -> Error ("Invalid type for binary operation", pos))
  | Uop (pos, up, ex) -> (
      let et = check_expr ex env in
      match et with
      | Ok cet -> (
          match (up, cet) with
          | (Not | Neg | Til), Dint -> Ok Dint
          | (Not | Neg | Til), Dfloat -> Ok Dfloat
          | _ -> Error ("Invalid type for uniary operation", pos))
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
          | _ -> Error ("Different argument count", pos)
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
            Error ("Different assignment types", pos)
          else
            Ok tl
      | _ -> Error ("Assignment type check error", pos))

let rec check_stmts stmt env fdt : (unit, string * pos) result =
  match stmt with
  | Ret (pos, ex) -> (
      match ex with
      | None ->
          if fdt <> Dvoid then
            Error ("Missing return value", pos)
          else
            Ok ()
      | Some e -> (
          match check_expr e env with
          | Ok dt ->
              if dt <> fdt then
                Error
                  ( "Return statment has different return type then defined in \
                     the function",
                    pos )
              else
                Ok ()
          | Error (e, p) -> Error (e, p)))
  | Expr (pos, ex) -> (
      match check_expr ex env with
      | Error (e, _) -> Error (e, pos)
      | _ -> Ok ())
  | Decl (pos, dname, decl_type, ex) -> (
      match Vars.add env.vars dname decl_type with
      | Error msg -> Error (msg, pos)
      | Ok _ -> (
          match ex with
          | None -> Ok ()
          | Some e -> (
              match check_expr e env with
              | Ok dt ->
                  if dt <> decl_type then
                    Error ("Type mismatch in assignment in declaration", pos)
                  else
                    Ok ()
              | Error (e, p) -> Error (e, p))))
  | If (pos, ex, stmtl, stmtlo) -> (
      match check_expr ex env with
      | Error (e, p) -> Error (e, p)
      | Ok dt -> (
          if dt = Dvoid then
            Error ("If condition does not take type void", pos)
          else
            let if_scope = { env with vars = Vars.push_scope env.vars } in
            let res = check_stmt_list stmtl if_scope fdt in
            match stmtlo with
            | None -> res
            | Some sl -> (
                match res with
                | Error (e, p) -> Error (e, p)
                | Ok () ->
                    let else_scope =
                      { env with vars = Vars.push_scope env.vars }
                    in
                    check_stmt_list sl else_scope fdt)))
  | While (pos, ex, stmtl) -> (
      match check_expr ex env with
      | Error (e, p) -> Error (e, p)
      | Ok dt -> (
          if dt = Dvoid then
            Error ("While condition does not take type void", pos)
          else
            let while_scope = { env with vars = Vars.push_scope env.vars } in
            let res = check_stmt_list stmtl while_scope fdt in
            match res with
            | Error (e, p) -> Error (e, p)
            | Ok () -> Ok ()))
  | Block (pos, stmtl) -> (
      let block_scope = { env with vars = Vars.push_scope env.vars } in
      match check_stmt_list stmtl block_scope fdt with
      | Error (e, _) -> Error (e, pos)
      | Ok () -> Ok ())

and check_stmt_list stmtl env fdt =
  match stmtl with
  | [] -> Ok ()
  | stmt :: stmr -> (
      match check_stmts stmt env fdt with
      | Ok () -> check_stmt_list stmr env fdt
      | Error (e, p) -> Error (e, p))

let check_prog prog =
  let env = creat_env () in

  let rec check_top_level env decls =
    match decls with
    | [] -> Ok ()
    | Global_Vars (pos, name, dt, expr_opt) :: rest -> (
        match expr_opt with
        | None -> (
            match Vars.add env.vars name dt with
            | Ok _ -> check_top_level env rest
            | Error msg -> Error (msg, pos))
        | Some e -> (
            match check_expr e env with
            | Error (e, p) -> Error (e, p)
            | Ok et -> (
                if et <> dt then
                  Error ("Global variable type mismatch", pos)
                else
                  match Vars.add env.vars name dt with
                  | Ok _ -> check_top_level env rest
                  | Error msg -> Error (msg, pos))))
    | Funcs f :: rest -> (
        let npos, fname = f.name in
        let _, rdt = f.ret_type in
        let params = List.map (fun (np, pdt, _) -> (np, pdt)) f.params in
        match Funcs.add env.fns fname rdt params with
        | Error msg -> Error (msg, npos)
        | Ok _ -> (
            let local_env = { env with vars = Vars.push_scope env.vars } in

            let rec add_params_to_lscope env = function
              | [] -> Ok env
              | (pname, pdt, ppos) :: rest -> (
                  match Vars.add env.vars pname pdt with
                  | Error msg -> Error (msg, ppos)
                  | Ok _ -> add_params_to_lscope env rest)
            in

            match add_params_to_lscope local_env f.params with
            | Error e -> Error e
            | Ok env_params -> (
                match check_stmt_list f.body env_params rdt with
                | Error e -> Error e
                | Ok () -> check_top_level env rest)))
  in
  check_top_level env prog
