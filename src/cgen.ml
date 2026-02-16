open Ast
open Parsed_ast
open Symb_table

let qbe_buf = Buffer.create 4096
let emit fmt = Printf.bprintf qbe_buf fmt
let ret_dtype = function Dint -> "w" | Dvoid -> "" | Dfloat -> "d"
let ret_vtype = function Lvars -> "%" | Gvars -> "$"
let tmp_cnt = ref 0
let label_id = ref 0
let func_cnt = ref 0

let make_tmp () =
  incr tmp_cnt;
  Printf.sprintf "%%t%d" !tmp_cnt

let make_label () =
  incr label_id;
  Printf.sprintf "@L%d" !label_id

let make_func () =
  incr func_cnt;
  Printf.sprintf "f%d" !func_cnt

let rec emit_exp ?(dt = "") ids_map (ids_map_func : (string, string) Hashtbl.t)
    env = function
  | Int (_, i) ->
      let t = make_tmp () in
      emit "%s =w copy %d\n" t i;
      (t, "w")
  | Float (_, f) ->
      let t = make_tmp () in
      emit "%s =d copy %s\n" t ("d_" ^ string_of_float f);
      (t, "d")
  | Id_name (_, name) -> (
      let allocd_name = Hashtbl.find ids_map name in
      let t = make_tmp () in
      match Vars.find env name with
      | Error e -> failwith ("Abort: Backend error : ID_NAME : " ^ e)
      | Ok (idt, _) ->
          let rdt = ret_dtype idt in
          emit "%s =%s load%s %s\n" t rdt rdt allocd_name;
          (t, rdt))
  | Assign (_, lhs, rhs) -> (
      let r, ret = emit_exp ids_map ids_map_func env rhs in
      match lhs with
      | Id_name (_, name) -> (
          let allocd_name = Hashtbl.find ids_map name in
          match Vars.find env name with
          | Error e -> failwith ("Abort : Backend error : Assign : " ^ e)
          | Ok (idt, _) ->
              let rdt = ret_dtype idt in
              emit "store%s %s, %s\n" rdt r allocd_name;
              (r, ret))
      | _ -> failwith "Abort: Backend error : Assign.")
  | Binop (_, bop, e1, e2) -> (
      let r1, ret = emit_exp ids_map ids_map_func env e1 in
      let r2, _ = emit_exp ids_map ids_map_func env e2 in
      let t = make_tmp () in
      match bop with
      | Eq ->
          emit "%s =w ceq%s %s, %s\n" t ret r1 r2;
          if ret = "d" then (
            let t2 = make_tmp () in
            emit "%s =d swtof %s" t2 t;
            (t2, ret))
          else (t, ret)
      | Neq ->
          emit "%s =w cne%s %s, %s\n" t ret r1 r2;
          if ret = "d" then (
            let t2 = make_tmp () in
            emit "%s =d swtof %s" t2 t;
            (t2, ret))
          else (t, ret)
      | Les ->
          if ret = "d" then (
            emit "%s =w cltd %s, %s\n" t r1 r2;
            let t2 = make_tmp () in
            emit "%s =d swtof %s" t2 t;
            (t2, ret))
          else (
            emit "%s =w csltw %s, %s\n" t r1 r2;
            (t, ret))
      | Grt ->
          if ret = "d" then (
            emit "%s =w cgtd %s, %s\n" t r1 r2;
            let t2 = make_tmp () in
            emit "%s =d swtof %s" t2 t;
            (t2, ret))
          else (
            emit "%s =w csgtw %s, %s\n" t r1 r2;
            (t, ret))
      | Lese ->
          if ret = "d" then (
            emit "%s =w cled %s, %s\n" t r1 r2;
            let t2 = make_tmp () in
            emit "%s =d swtof %s" t2 t;
            (t2, ret))
          else (
            emit "%s =w cslew %s, %s\n" t r1 r2;
            (t, ret))
      | Grte ->
          if ret = "d" then (
            emit "%s =w cged %s, %s\n" t r1 r2;
            let t2 = make_tmp () in
            emit "%s =d swtof %s" t2 t;
            (t2, ret))
          else (
            emit "%s =w csgew %s, %s\n" t r1 r2;
            (t, ret))
      | Add ->
          emit "%s =%s add %s, %s\n" t ret r1 r2;
          (t, ret)
      | Mul ->
          emit "%s =%s mul %s, %s\n" t ret r1 r2;
          (t, ret)
      | Sub ->
          emit "%s =%s sub %s, %s\n" t ret r1 r2;
          (t, ret)
      | Div ->
          emit "%s =%s div %s, %s\n" t ret r1 r2;
          (t, ret)
      | Mod ->
          if ret = "d" then (
            let t2 = make_tmp () in
            emit "%s =w dtosi %s\n" t r1;
            emit "%s =w dtosi %s\n" t2 r2;
            let t3 = make_tmp () in
            emit "%s =w rem %s, %s\n" t3 t t2;
            let t4 = make_tmp () in
            emit "%s =d swtof %s\n" t4 t3;
            (t4, ret))
          else (
            emit "%s =w rem %s, %s\n" t r1 r2;
            (t, ret))
      | And ->
          let from = make_label () in
          let right = make_label () in
          let join = make_label () in
          emit "%s\n" from;
          if ret = "d" then (
            let t2 = make_tmp () in
            emit "%s =w cned %s, d_0\n" t2 r1;
            emit "jnz %s, %s, %s\n" t2 right join;
            emit "%s\n" right;
            let t3 = make_tmp () in
            emit "%s =w cned %s, d_0\n" t3 r2;
            emit "%s\n" join;
            let t4 = make_tmp () in
            emit "%s =w phi %s 0, %s %s\n" t4 from right t3;
            let t5 = make_tmp () in
            emit "%s =d swtof %s\n" t5 t4;
            (t5, ret))
          else (
            emit "jnz %s, %s, %s\n" r1 right join;
            emit "%s\n" right;
            let t2 = make_tmp () in
            emit "%s =w cnew %s, 0\n" t2 r2;
            emit "%s\n" join;
            let t3 = make_tmp () in
            emit "%s =w phi %s 0, %s %s\n" t3 from right t2;
            (t3, ret))
      | Or ->
          let from = make_label () in
          let right = make_label () in
          let join = make_label () in
          emit "%s\n" from;
          if ret = "d" then (
            let t2 = make_tmp () in
            emit "%s =w cned %s, d_0\n" t2 r1;
            emit "jnz %s, %s, %s\n" t2 join right;
            emit "%s\n" right;
            let t3 = make_tmp () in
            emit "%s =w cned %s, d_0\n" t3 r2;
            emit "%s\n" join;
            let t4 = make_tmp () in
            emit "%s =w phi %s 1, %s %s\n" t4 from right t3;
            let t5 = make_tmp () in
            emit "%s =d swtof %s\n" t5 t4;
            (t5, ret))
          else (
            emit "jnz %s, %s, %s\n" r1 join right;
            emit "%s\n" right;
            let t2 = make_tmp () in
            emit "%s =w cnew %s, 0\n" t2 r2;
            emit "%s\n" join;
            let t3 = make_tmp () in
            emit "%s =w phi %s 1, %s %s\n" t3 from right t2;
            (t3, ret)))
  | Uop (_, uop, e) -> (
      let r, ret = emit_exp ids_map ids_map_func env e in
      let t1 = make_tmp () in
      match uop with
      | Not ->
          if ret = "d" then (
            emit "%s =d swtof 0\n" t1;
            let t2 = make_tmp () in
            emit "%s =w ceqd %s, %s\n" t2 r t1;
            let t3 = make_tmp () in
            emit "%s =d swtof %s\n" t3 t2;
            (t3, ret))
          else (
            emit "%s =w ceqw %s, 0\n" t1 r;
            (t1, ret))
      | Neg ->
          if ret = "d" then (
            emit "%s =d neg %s\n" t1 r;
            (t1, ret))
          else (
            emit "%s =w neg %s\n" t1 r;
            (t1, ret))
      | Til ->
          if ret = "d" then (
            emit "%s =w dtosi %s" t1 r;
            let t2 = make_tmp () in
            emit "%s =w xor %s, 18446744073709551615\n" t2 t1;
            let t3 = make_tmp () in
            emit "%s =d swtof %s\n" t3 t2;
            (t1, ret))
          else (
            emit "%s =w xor %s, 18446744073709551615\n" t1 r;
            (t1, ret)))
  | Call (_, fname, args) ->
      let ir_func_name = Hashtbl.find ids_map_func fname in
      let args_t =
        List.map (fun e -> emit_exp ids_map ids_map_func env e) args
      in
      let t = make_tmp () in
      emit "%s =%s call $%s(%s)\n" t dt ir_func_name
        (String.concat ", "
           (List.map (fun (arg, pty) -> pty ^ " " ^ arg) args_t));
      (t, dt)

let ret_align_size = function
  | Dint -> 4
  | Dfloat -> 8
  | _ ->
      failwith
        "Abort : Backend error : Alignment size of unknow type (most probably \
         void) was asked : Sema error."

let ret_constants const =
  match const with
  | Int (_, i) -> string_of_int i
  | Float (_, f) -> "d_" ^ string_of_float f
  | _ ->
      failwith
        "Abort: Backend error : Sema did not check that GVars can only take \
         const expressions."

(* TODO: make expressions evaluator for gvars, also a kind of comptime execution
 system: say gvars1 = some_number, and gvars2 = gvars1, make this work*)
let emit_gvar name ty eopt ids_map =
  Hashtbl.add ids_map name ("$" ^ name);
  match eopt with
  | None -> emit "data $%s = align 4 { z 4 }\n" name
  | Some e ->
      emit "data $%s = align %d { %s %s }\n" name (ret_align_size ty)
        (ret_dtype ty) (ret_constants e)

let rec emit_stmts ids_map ids_map_func env = function
  | PRet None -> emit "ret\n"
  | PRet (Some e) ->
      let r, _ = emit_exp ids_map ids_map_func env e in
      emit "ret %s\n" r
  | PIf (if_e, if_stmts, else_stmts) ->
      let if_local_map = Hashtbl.copy ids_map in
      let r, _ = emit_exp if_local_map ids_map_func if_stmts.bscope if_e in
      let if_l = make_label () in
      let else_l = make_label () in
      let end_l = make_label () in
      emit "jnz %s, %s, %s\n" r if_l else_l;
      emit "%s\n" if_l;
      List.iter
        (emit_stmts if_local_map ids_map_func if_stmts.bscope)
        if_stmts.stmts;
      emit "jmp %s\n" end_l;
      emit "%s\n" else_l;
      Option.iter
        (fun block ->
          List.iter (emit_stmts if_local_map ids_map_func env) block.stmts)
        else_stmts;
      emit "%s\n" end_l
  | PWhile (e, stmts) ->
      let while_local_map = Hashtbl.copy ids_map in
      let cond_l = make_label () in
      let body_l = make_label () in
      let end_l = make_label () in
      emit "jmp %s\n" cond_l;
      emit "%s\n" cond_l;
      let r, _ = emit_exp while_local_map ids_map_func stmts.bscope e in
      emit "jnz %s, %s, %s\n" r body_l end_l;
      emit "%s\n" body_l;
      List.iter (emit_stmts while_local_map ids_map_func env) stmts.stmts;
      emit "jmp %s\n" cond_l;
      emit "%s\n" end_l
  | PDecl (name, _, eopt) -> (
      let t = make_tmp () in
      match Vars.find env name with
      | Error e -> failwith ("Abort : Backend error : PDECL : " ^ e)
      | Ok (dt, _) ->
          let align = ret_align_size dt in
          emit "%s =l alloc%d %d\n" t align align;
          Hashtbl.add ids_map name t;
          Option.iter
            (fun e ->
              let r, ret =
                emit_exp ~dt:(ret_dtype dt) ids_map ids_map_func env e
              in
              emit "store%s %s, %s\n" ret r t)
            eopt)
  | PBlock stmts ->
      let block_local_map = Hashtbl.copy ids_map in
      List.iter (emit_stmts block_local_map ids_map_func env) stmts.stmts
  | PExpr e -> ignore (emit_exp ids_map ids_map_func env e)

let emit_func ids_map ids_map_func f =
  let func_local_map = Hashtbl.copy ids_map in
  let params =
    List.map
      (fun (name, ty) ->
        let t = make_tmp () in
        Hashtbl.add func_local_map name t;
        Printf.sprintf "%s %s" (ret_dtype ty) t)
      f.pparams
  in
  let ir_func_name = Hashtbl.find ids_map_func f.pname in
  if f.pname = "main" then emit "export ";
  emit "function %s $%s(%s) {\n" (ret_dtype f.pret_type) ir_func_name
    (String.concat ", " params);
  emit "@start\n";
  List.iter
    (emit_stmts func_local_map ids_map_func f.pbody.bscope)
    f.pbody.stmts;
  emit "}\n"

let emit_program =
  let ids_map = Hashtbl.create 4 in
  let ids_map_func = Hashtbl.create 4 in
  function
  | PFuncs f ->
      if f.pname = "main" then begin
        Hashtbl.add ids_map_func f.pname f.pname
      end
      else begin
        let f' = make_func () in
        Hashtbl.add ids_map_func f.pname f'
      end;
      emit_func ids_map ids_map_func f
  | PGlobal_Vars (name, ty, eopt) -> emit_gvar name ty eopt ids_map

let emitter_driver past =
  List.iter emit_program past;
  Buffer.contents qbe_buf
