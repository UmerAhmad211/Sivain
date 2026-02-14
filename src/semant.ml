open Ast
open Parsed_ast
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
  | Id_name (pos, name) -> (
      match Vars.find env.vars name with
      | Error e -> Error (e, pos)
      | Ok (dt, _) -> Ok dt)
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
                  else check_args_type rparams rexs
              | Error (er, p) -> Error (er, p))
          | _ -> Error ("Different argument count", pos)
        in
        check_args_type params exs
      else Error ("Unknown function name " ^ fname, pos)
  | Assign (pos, el, er) -> (
      let elt = check_expr el env in
      let ert = check_expr er env in
      match (elt, ert) with
      | Ok tl, Ok tr -> (
          if tl <> tr then Error ("Different assignment types", pos)
          else
            match el with
            | Id_name (_, _) -> Ok tl
            | _ -> Error ("Assignment error", pos))
      | _ -> Error ("Assignment type check error", pos))

(*
  @brief: checks statements,
  returns a parsed AST which has some
  information stripped for code generation
 *)
let rec check_stmts stmt env fdt : (parsed_stmt, string * pos) result =
  match stmt with
  | Ret (pos, ex) -> (
      match ex with
      | None ->
          if fdt <> Dvoid then Error ("Missing return value", pos)
          else Ok (PRet None)
      | Some e -> (
          match check_expr e env with
          | Ok dt ->
              if dt <> fdt then
                Error
                  ( "Return statment has different return type then defined in \
                     the function",
                    pos )
              else Ok (PRet (Some e))
          | Error (e, p) -> Error (e, p)))
  | Expr (pos, ex) -> (
      match check_expr ex env with
      | Error (e, _) -> Error (e, pos)
      | Ok _ -> Ok (PExpr ex))
  | Decl (pos, dname, decl_type, ex) -> (
      if decl_type = Dvoid then Error ("Void data type not allowed", pos)
      else
        match Vars.add env.vars dname decl_type Lvars with
        | Error msg -> Error (msg, pos)
        | Ok _ -> (
            match ex with
            | None -> Ok (PDecl (dname, decl_type, None))
            | Some e -> (
                match check_expr e env with
                | Ok dt ->
                    if dt <> decl_type then
                      Error ("Type mismatch in assignment in declaration", pos)
                    else Ok (PDecl (dname, decl_type, Some e))
                | Error (e, p) -> Error (e, p))))
  | If (pos, ex, stmtl, stmtlo) -> (
      match check_expr ex env with
      | Error (e, p) -> Error (e, p)
      | Ok dt -> (
          if dt = Dvoid then Error ("If condition does not take type void", pos)
          else
            let if_scope = { env with vars = Vars.push_scope env.vars } in
            let res = check_stmt_list stmtl if_scope fdt in
            match stmtlo with
            | None -> (
                match res with
                | Error (e, p) -> Error (e, p)
                | Ok psl ->
                    Ok (PIf (ex, { bscope = if_scope.vars; stmts = psl }, None))
                )
            | Some sl -> (
                match res with
                | Error (e, p) -> Error (e, p)
                | Ok psl -> (
                    let else_scope =
                      { env with vars = Vars.push_scope env.vars }
                    in
                    let ptsl = check_stmt_list sl else_scope fdt in
                    match ptsl with
                    | Error (e, p) -> Error (e, p)
                    | Ok elpsl ->
                        Ok
                          (PIf
                             ( ex,
                               { bscope = else_scope.vars; stmts = psl },
                               Some { bscope = else_scope.vars; stmts = elpsl }
                             ))))))
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
            | Ok psl ->
                Ok (PWhile (ex, { bscope = while_scope.vars; stmts = psl }))))
  | Block (pos, stmtl) -> (
      let block_scope = { env with vars = Vars.push_scope env.vars } in
      match check_stmt_list stmtl block_scope fdt with
      | Error (e, _) -> Error (e, pos)
      | Ok psl -> Ok (PBlock { bscope = block_scope.vars; stmts = psl }))

and check_stmt_list stmtl env fdt : (parsed_stmt list, string * pos) result =
  match stmtl with
  | [] -> Ok []
  | stmt :: stmr -> (
      match check_stmts stmt env fdt with
      | Ok pstmt -> (
          match check_stmt_list stmr env fdt with
          | Error e -> Error e
          | Ok prest -> Ok (pstmt :: prest))
      | Error (e, p) -> Error (e, p))

let check_prog prog =
  let env = creat_env () in
  let past : parsed_prog = [] in

  let rec check_top_level env decls tast =
    match decls with
    | [] -> Ok tast
    | Global_Vars (pos, name, dt, expr_opt) :: rest -> (
        match expr_opt with
        | None -> (
            match Vars.add env.vars name dt Gvars with
            | Ok _ ->
                let pgvar = PGlobal_Vars (name, dt, None) in
                let nlist = pgvar :: tast in
                check_top_level env rest nlist
            | Error msg -> Error (msg, pos))
        | Some e -> (
            match check_expr e env with
            | Error (e, p) -> Error (e, p)
            | Ok et -> (
                if et <> dt then Error ("Global variable type mismatch", pos)
                else
                  match Vars.add env.vars name dt Gvars with
                  | Ok _ ->
                      let pgvar = PGlobal_Vars (name, dt, Some e) in
                      let nlist = pgvar :: tast in
                      check_top_level env rest nlist
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
                  match Vars.add env.vars pname pdt Lvars with
                  | Error msg -> Error (msg, ppos)
                  | Ok _ -> add_params_to_lscope env rest)
            in

            match add_params_to_lscope local_env f.params with
            | Error e -> Error e
            | Ok env_params -> (
                match check_stmt_list f.body env_params rdt with
                | Error e -> Error e
                | Ok psl ->
                    let pfunc =
                      PFuncs
                        {
                          pname = fname;
                          pparams = params;
                          pret_type = rdt;
                          pbody = { bscope = local_env.vars; stmts = psl };
                        }
                    in
                    let nlist = pfunc :: tast in
                    check_top_level env rest nlist)))
  in
  check_top_level env prog past
