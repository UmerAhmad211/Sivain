open Util
open Semant
open Cgen
open Cmdliner
open Cmdliner.Term.Syntax

type target = Amd64_sysv | Arm64 | Rv64

let valid_targets =
  [ ("amd64_sysv", Amd64_sysv); ("arm64", Arm64); ("rv64", Rv64) ]

let string_of_target = function
  | Amd64_sysv -> "amd64_sysv"
  | Arm64 -> "arm64"
  | Rv64 -> "rv64"

let build_dir = ".siv-build"

let run_comm comm args =
  let pid = Unix.create_process comm args Unix.stdin Unix.stderr Unix.stdout in
  let _, status = Unix.waitpid [] pid in
  match status with Unix.WEXITED 0 -> () | _ -> failwith (comm ^ "failed.")

let compile ~target ~cc ~file ~output =
  if not (check_filename_exe file) then
    `Error (true, file ^ ": Sivain files end with .svn.")
  else if not (Sys.file_exists file) then
    `Error (true, file ^ " no such file exists.")
  else
    `Ok
      (let ic = open_in file in
       let lexbuf = Lexing.from_channel ic in
       let ast =
         try Parser.program Lexer.read_token lexbuf with
         | Lexer.Syntax_Error msg ->
             prerr_endline msg;
             exit 1
         | Parser.Error ->
             Printf.eprintf "Sivain: Error: Syntax error at %d:%d\n"
               lexbuf.lex_curr_p.pos_lnum
               (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
             exit 1
       in
       close_in ic;
       check_for_main ast;
       match check_prog ast with
       | Ok past ->
           let fixed_past = List.rev past in
           let cgened = emitter_driver fixed_past in
           if not (Sys.file_exists build_dir) then Unix.mkdir build_dir 0o755;
           let ssa_fname =
             (build_dir ^ "/"
             ^ (file |> Filename.basename |> Filename.remove_extension))
             ^ ".ssa"
           in
           let asm_fname = build_dir ^ "/" ^ output ^ ".s" in
           let oc = open_out ssa_fname in
           output_string oc cgened;
           flush oc;
           close_out oc;
           run_comm "qbe" [| "qbe"; ssa_fname; "-t"; target; "-o"; asm_fname |];
           run_comm cc [| cc; asm_fname; "-o"; output |]
       | Error (msg, pos) ->
           Printf.eprintf "Sivain: Error: %s at %d:%d.\n" msg pos.pos_lnum
             (pos.pos_cnum - pos.pos_bol);
           exit 1)

let cc =
  let doc = "C compiler to compile code to target architecture." in
  Arg.(value & opt string "cc" & info [ "cc" ] ~doc ~docv:"CC")

let target =
  let doc =
    "Target architecture: $(docv) (Available: Amd64 Sys-V, ARM64 and RV64. \
     Defaults to system architecture)."
  in
  let targ_enum = Arg.enum valid_targets in
  Arg.(
    value & opt targ_enum Amd64_sysv
    & info [ "t"; "target" ] ~doc ~docv:"TARGET")

let file =
  let doc = "Input source file." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FILE")

let output =
  let doc = "Output executable name." in
  Arg.(required & opt (some string) None & info [ "o" ] ~doc ~docv:"OUTPUT")

let svn_cmd =
  let doc = "Compiler for the Sivain language." in
  let man =
    [
      `S Manpage.s_bugs;
      `P "Open bug reports in <https://github.com/UmerAhmad211/Sivain/issues>.";
    ]
  in
  Cmd.make (Cmd.info "svc" ~version:"0.0.1" ~doc ~man)
  @@ Term.ret
  @@
  let+ target = target and+ cc = cc and+ file = file and+ output = output in
  compile ~target:(string_of_target target) ~cc ~file ~output

let main () = Cmd.eval svn_cmd
let () = if !Sys.interactive then () else exit (main ())
