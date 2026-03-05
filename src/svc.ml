open Util
open Semant
open Cmdliner
open Cmdliner.Term.Syntax

let compile ~target ~cc ~file ~output ~backend =
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
           Cgen_driver.driver fixed_past target cc file output backend
       | Error (msg, pos) ->
           Printf.eprintf "Sivain: Error: %s at %d:%d.\n" msg pos.pos_lnum
             (pos.pos_cnum - pos.pos_bol);
           exit 1)

let cc =
  let doc = "C compiler to compile code to target architecture." in
  Arg.(value & opt string "cc" & info [ "cc" ] ~doc ~docv:"CC")

let backend =
  let doc = "Code generation backend to use. Available: QBE and LLVM." in
  let backend_enum = Arg.enum valid_backends in
  Arg.(value & opt backend_enum Qbe & info [ "backend" ] ~doc ~docv:"BACKEND")

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
  let+ target = target
  and+ cc = cc
  and+ file = file
  and+ output = output
  and+ backend = backend in
  compile ~target ~cc ~file ~output ~backend

let main () = Cmd.eval svn_cmd
let () = if !Sys.interactive then () else exit (main ())
