let usage_msg = "Usage: svc [-target=<amd64|arm64|riscv64>] <file1> -o <output>"
let input_file = ref ""
let output_file = ref ""
let target_arch = ref "amd64"
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
  else (
    prerr_endline "Error: Only one file expected.";
    exit 1)

let check_target tg =
  if List.mem tg valid_target then
    target_arch := tg
  else (
    prerr_endline ("Error: Invalid target architecture: " ^ tg);
    exit 1)

let speclist =
  [
    ( "-target",
      Arg.String check_target,
      "Set target architecture (amd64, arm64, riscv64)." );
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !input_file = "" then (
    prerr_endline "Error: Input file expected.";
    exit 1);
  let ic = open_in !input_file in
  let lexbuf = Lexing.from_channel ic in
  let ast =
    try Parser.program Lexer.read_token lexbuf
    with Parser.Error ->
      Printf.eprintf "Syntax error at %d:%d\n" lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
      exit 1
  in
  close_in ic;

  let oc = open_out "temp.ssa" in
  Cgen.emit_program oc ast;
  close_out oc;

  run_comm "qbe" [| "qbe"; "temp.ssa"; "-o"; "t" ^ !output_file ^ ".s" |];
  run_comm "cc" [| "cc"; "t" ^ !output_file ^ ".s"; "-o"; !output_file |]
