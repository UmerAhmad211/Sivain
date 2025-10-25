open Util
open Semant

let usage_msg =
  "Usage: svc [--target=<amd64_sysv|arm64|rv64>] <file1> -o <output>"

let input_file = ref ""
let build_dir = ".siv-build"
let output_file = ref ""
let target_arch = ref "amd64_sysv"
let valid_target = [ "amd64"; "arm64"; "riscv64" ]

let run_comm comm args =
  let pid = Unix.create_process comm args Unix.stdin Unix.stderr Unix.stdout in
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith (comm ^ "failed.")

let anon_fun filename =
  if !input_file = "" then
    input_file := filename
  else
    print_err_usage_fail ~usage:usage_msg
      "Sivain: Error: Only one file expected."

let check_target tg =
  if List.mem tg valid_target then
    target_arch := tg
  else
    print_err_usage_fail ~usage:usage_msg
      ("Sivain: Error: Invalid target architecture: " ^ tg)

let speclist =
  [
    ( "--target",
      Arg.String check_target,
      "Set target architecture (amd64_sysv, arm64, rv64)." );
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !input_file = "" then
    print_err_usage_fail ~usage:usage_msg "Sivain: Error: Input file expected"
  else if not (check_filename_exe !input_file) then
    print_err_usage_fail "Sivain: Error: Invalid file extension.";

  let ic = open_in !input_file in
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
  (match check_prog ast with
  | Ok () -> ()
  | Error (msg, pos) ->
      Printf.eprintf "Sivain: Error: %s at %d:%d.\n" msg pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      exit 1);

  if not (Sys.file_exists build_dir) then
    Unix.mkdir build_dir 0o755;

  let ssa_fname =
    (build_dir ^ "/"
    ^ (!input_file |> Filename.basename |> Filename.remove_extension))
    ^ ".ssa"
  in
  let asm_fname = build_dir ^ "/" ^ !output_file ^ ".s" in
  let oc = open_out ssa_fname in
  Cgen.emit_program oc ast;
  flush oc;
  close_out oc;
  run_comm "qbe" [| "qbe"; ssa_fname; "-t"; !target_arch; "-o"; asm_fname |];
  run_comm "cc" [| "cc"; asm_fname; "-o"; !output_file |]
