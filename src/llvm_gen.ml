open Llvm
open Llvm_target
open Ast
open Parsed_ast
open Symb_table

let ctxt = global_context ()
let module' = create_module ctxt "svn"
let builder' = builder ctxt
let int_ty = i32_type ctxt
let double_ty = double_type ctxt
let void_ty = void_type ctxt;;

Llvm_all_backends.initialize ();;

let tt = Target.default_triple ()
let targ = Target.by_triple tt
let cpu = "generic"
let feats = ""

let tm =
  TargetMachine.create ~triple:tt ~cpu ~features:feats targ
    ~level:CodeGenOptLevel.Default

let ret_lltype = function
  | Dint -> int_ty
  | Dfloat -> double_ty
  | Dvoid -> void_ty

let create_alloca function' name ty =
  let builder'' = builder_at ctxt (instr_begin (entry_block function')) in
  build_alloca ty name builder''

let rec emit_exp named_values frt (env : scope) = function
  | Int (_, i) -> const_int int_ty i
  | Float (_, f) -> const_float double_ty f
  | Id_name (_, name) -> (
      let v = Hashtbl.find named_values name in
      match Vars.find env name with
      | Error e -> failwith ("Abort: Backend error : ID_NAME : " ^ e)
      | Ok (idt, _) ->
          let rdt = ret_lltype idt in
          build_load rdt v name builder')
  | Assign (_, lhs, rhs) -> (
      match lhs with
      | Id_name (_, name) -> (
          match Vars.find env name with
          | Error e -> failwith ("Abort : Backend error : Assign : " ^ e)
          | Ok _ ->
              let p = Hashtbl.find named_values name in
              let v = emit_exp named_values frt env rhs in
              build_store v p builder')
      | _ -> failwith "Abort: Backend error : Assign.")
  | Call (_, fname, args) ->
      let callee =
        match lookup_function fname module' with
        | Some callee -> callee
        | None -> failwith "Abort : Backend error : Call."
      in
      let args_arr = Array.of_list args in
      let args' = Array.map (emit_exp named_values frt env) args_arr in
      let args_ty = Array.map type_of args' in
      let fnty = function_type frt args_ty in
      build_call fnty callee args' "calltmp" builder'
  | Binop (_, bop, e1, e2) -> (
      let r1 = emit_exp named_values frt env e1 in
      let te = type_of r1 in

      match bop with
      | Eq ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then
            let cmp_ret = build_icmp Icmp.Eq r1 r2 "eqtmp" builder' in
            build_zext cmp_ret int_ty "ibool" builder'
          else
            let cmp_ret = build_fcmp Fcmp.Oeq r1 r2 "feqtmp" builder' in
            build_sitofp cmp_ret double_ty "fbool" builder'
      | Neq ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then
            let cmp_ret = build_icmp Icmp.Ne r1 r2 "neqtmp" builder' in
            build_zext cmp_ret int_ty "ibool" builder'
          else
            let cmp_ret = build_fcmp Fcmp.One r1 r2 "fneqtmp" builder' in
            build_sitofp cmp_ret double_ty "fbool" builder'
      | Les ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then
            let cmp_ret = build_icmp Icmp.Slt r1 r2 "letmp" builder' in
            build_zext cmp_ret int_ty "ibool" builder'
          else
            let cmp_ret = build_fcmp Fcmp.Olt r1 r2 "fletmp" builder' in
            build_sitofp cmp_ret double_ty "fbool" builder'
      | Grt ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then
            let cmp_ret = build_icmp Icmp.Sgt r1 r2 "grtmp" builder' in
            build_zext cmp_ret int_ty "ibool" builder'
          else
            let cmp_ret = build_fcmp Fcmp.Ogt r1 r2 "fgrtmp" builder' in
            build_sitofp cmp_ret double_ty "fbool" builder'
      | Lese ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then
            let cmp_ret = build_icmp Icmp.Sle r1 r2 "lesetmp" builder' in
            build_zext cmp_ret int_ty "ibool" builder'
          else
            let cmp_ret = build_fcmp Fcmp.Ole r1 r2 "flesetmp" builder' in
            build_sitofp cmp_ret double_ty "fbool" builder'
      | Grte ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then
            let cmp_ret = build_icmp Icmp.Sge r1 r2 "egrtetmp" builder' in
            build_zext cmp_ret int_ty "ibool" builder'
          else
            let cmp_ret = build_fcmp Fcmp.Oge r1 r2 "fgrtetmp" builder' in
            build_sitofp cmp_ret double_ty "fbool" builder'
      | Add ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then build_add r1 r2 "addtmp" builder'
          else build_fadd r1 r2 "faddtmp" builder'
      | Mul ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then build_mul r1 r2 "multmp" builder'
          else build_mul r1 r2 "fmultmp" builder'
      | Sub ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then build_sub r1 r2 "subtmp" builder'
          else build_fsub r1 r2 "fsubtmp" builder'
      | Div ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then build_sdiv r1 r2 "divtmp" builder'
          else build_fdiv r1 r2 "fdivtmp" builder'
      | Mod ->
          let r2 = emit_exp named_values frt env e2 in
          if te = int_ty then build_srem r1 r2 "remtmp" builder'
          else build_frem r1 r2 "fremtmp" builder'
      | And ->
          let curr_point = insertion_block builder' in
          let curr_fn = block_parent curr_point in
          let right = append_block ctxt "r_bb" curr_fn in
          let join = append_block ctxt "join_bb" curr_fn in
          let te = type_of r1 in
          let bool_r1 =
            if te = int_ty then
              build_icmp Icmp.Ne r1 (const_int int_ty 0) "boolcond" builder'
            else
              build_fcmp Fcmp.One r1
                (const_float double_ty 0.0)
                "boolcond" builder'
          in
          ignore (build_cond_br bool_r1 right join builder');
          position_at_end right builder';
          let r2 = emit_exp named_values frt env e2 in
          ignore (build_br join builder');
          let right_end = insertion_block builder' in
          position_at_end join builder';
          if te = int_ty then
            let phi =
              build_phi
                [ (const_int int_ty 0, curr_point); (r2, right_end) ]
                "andtmp" builder'
            in
            phi
          else
            let phi =
              build_phi
                [ (const_float double_ty 0.0, curr_point); (r2, right_end) ]
                "andtmp" builder'
            in
            phi
      | Or ->
          let curr_point = insertion_block builder' in
          let curr_fn = block_parent curr_point in
          let right = append_block ctxt "r_bb" curr_fn in
          let join = append_block ctxt "join_bb" curr_fn in
          let te = type_of r1 in
          let bool_r1 =
            if te = int_ty then
              build_icmp Icmp.Ne r1 (const_int int_ty 0) "boolcond" builder'
            else
              build_fcmp Fcmp.One r1
                (const_float double_ty 0.0)
                "boolcond" builder'
          in
          ignore (build_cond_br bool_r1 join right builder');
          position_at_end right builder';
          let r2 = emit_exp named_values frt env e2 in
          ignore (build_br join builder');
          let right_end = insertion_block builder' in
          position_at_end join builder';
          if te = int_ty then
            let phi =
              build_phi
                [ (const_int int_ty 1, curr_point); (r2, right_end) ]
                "ortmp" builder'
            in
            phi
          else
            let phi =
              build_phi
                [ (const_float double_ty 1.0, curr_point); (r2, right_end) ]
                "ortmp" builder'
            in
            phi)
  | Uop (_, uop, e) -> (
      let r = emit_exp named_values frt env e in
      let te = type_of r in
      match uop with
      | Not ->
          if te = int_ty then
            let zero = const_int int_ty 0 in
            build_icmp Icmp.Eq r zero "notmp" builder'
          else
            let zero = const_float double_ty 0.0 in
            build_fcmp Fcmp.Oeq r zero "notmp" builder'
      | Neg ->
          if te = int_ty then build_neg r "negtmp" builder'
          else build_fneg r "negtmp" builder'
      | Til ->
          if te = int_ty then build_not r "tiltmp" builder'
          else
            let casted_double = build_fptosi r double_ty "dtoi" builder' in
            let val' = build_not casted_double "tiltmp" builder' in
            build_sitofp val' int_ty "itod" builder')

let rec emit_stmts named_values frt env = function
  | PRet None -> ignore (build_ret_void builder')
  | PRet (Some e) ->
      let v = emit_exp named_values frt env e in
      ignore (build_ret v builder')
  | PDecl (name, dt, eopt) -> (
      match Vars.find env name with
      | Error e -> failwith ("Abort : Backend error : PDECL : " ^ e)
      | Ok _ -> (
          let function' = block_parent (insertion_block builder') in
          let rdt = ret_lltype dt in
          let alloca = create_alloca function' name rdt in
          Hashtbl.add named_values name alloca;
          match eopt with
          | None -> ()
          | Some e ->
              let v = emit_exp named_values (ret_lltype dt) env e in
              ignore (build_store v alloca builder')))
  | PExpr e -> ignore (emit_exp named_values frt env e)
  | PIf (if_e, if_stmts, else_stmts) ->
      let if_local_map = Hashtbl.copy named_values in
      let curr_point = insertion_block builder' in
      let curr_fn = block_parent curr_point in
      let if_true = append_block ctxt "if_true" curr_fn in
      let if_false = append_block ctxt "if_false" curr_fn in
      let if_exit = append_block ctxt "if_exit" curr_fn in

      let r = emit_exp if_local_map frt env if_e in
      let te = type_of r in
      let bool_r =
        if te = int_ty then
          build_icmp Icmp.Ne r (const_int int_ty 0) "ifcond" builder'
        else build_fcmp Fcmp.One r (const_float double_ty 0.0) "ifcond" builder'
      in
      ignore (build_cond_br bool_r if_true if_false builder');
      position_at_end if_true builder';
      List.iter (emit_stmts if_local_map frt if_stmts.bscope) if_stmts.stmts;
      ignore (build_br if_exit builder');
      position_at_end if_false builder';
      Option.iter
        (fun block ->
          List.iter (emit_stmts if_local_map frt block.bscope) block.stmts)
        else_stmts;
      ignore (build_br if_exit builder');
      position_at_end if_exit builder'
  | PWhile (e, stmts) ->
      let while_local_map = Hashtbl.copy named_values in
      let curr_point = insertion_block builder' in
      let curr_fn = block_parent curr_point in
      let while_cond = append_block ctxt "while_cond" curr_fn in
      let while_body = append_block ctxt "while_body" curr_fn in
      let while_exit = append_block ctxt "while_exit" curr_fn in

      ignore (build_br while_cond builder');
      position_at_end while_cond builder';

      let r = emit_exp while_local_map frt env e in
      let te = type_of r in
      let bool_r1 =
        if te = int_ty then
          build_icmp Icmp.Ne r (const_int int_ty 0) "ifcond" builder'
        else build_fcmp Fcmp.One r (const_float double_ty 0.0) "ifcond" builder'
      in
      ignore (build_cond_br bool_r1 while_body while_exit builder');
      position_at_end while_body builder';
      List.iter (emit_stmts while_local_map frt stmts.bscope) stmts.stmts;
      ignore (build_br while_cond builder');
      position_at_end while_exit builder'
  | PBlock stmts ->
      let block_local_scope = Hashtbl.copy named_values in
      List.iter (emit_stmts block_local_scope frt stmts.bscope) stmts.stmts

let emit_func named_values_all f =
  let named_values = Hashtbl.copy named_values_all in
  let params_arr = Array.of_list f.pparams in
  let params_ty = Array.map (fun (_, pt) -> ret_lltype pt) params_arr in
  let ret_ty = ret_lltype f.pret_type in
  let fn_type = function_type ret_ty params_ty in
  let fn_def = define_function f.pname fn_type module' in
  let params_name_arr = Array.map (fun (pname', _) -> pname') params_arr in

  let bb = entry_block fn_def in
  position_at_end bb builder';

  Array.iteri
    (fun i v ->
      let n = params_name_arr.(i) in
      set_value_name n v;
      let pty = params_ty.(i) in
      let alloca = create_alloca fn_def n pty in
      ignore (build_store v alloca builder');
      Hashtbl.add named_values n alloca)
    (params fn_def);

  List.iter
    (fun stmt -> emit_stmts named_values ret_ty f.pbody.bscope stmt)
    f.pbody.stmts

let ret_constants = function
  | Int (_, i) -> const_int int_ty i
  | Float (_, f) -> const_float double_ty f
  | _ ->
      failwith
        "Abort: Backend error : Sema did not check that GVars can only take \
         const expressions."

let emit_program =
  let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10 in
  function
  | PFuncs f -> emit_func named_values f
  | PGlobal_Vars (name, ty, eopt) -> (
      match eopt with
      | None ->
          let const =
            if ty = Dint then const_int int_ty 0 else const_float double_ty 0.0
          in
          let glob = define_global name const module' in
          Hashtbl.add named_values name glob
      | Some e ->
          let const = ret_constants e in
          let glob = define_global name const module' in
          Hashtbl.add named_values name glob)

let emitter_driver past =
  List.iter emit_program past;
  (match Llvm_analysis.verify_module module' with
  | Some output -> print_endline output
  | None -> ());
  string_of_llmodule module'
