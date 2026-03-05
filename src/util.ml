open Ast

type target = Amd64_sysv | Arm64 | Rv64
type backends = Qbe | Llvm

let valid_targets =
  [ ("amd64_sysv", Amd64_sysv); ("arm64", Arm64); ("rv64", Rv64) ]

let valid_backends = [ ("QBE", Qbe); ("LLVM", Llvm) ]

let string_of_target_qbe = function
  | Amd64_sysv -> "amd64_sysv"
  | Arm64 -> "arm64"
  | Rv64 -> "rv64"

let string_of_target_llvm = function
  | Amd64_sysv -> "--target=x86_64-pc-linux-gnu"
  | Arm64 -> "--target=aarch64-pc-linux-gnu"
  | Rv64 -> "--target=riscv64-unknown-linux-gnu"

let check_for_main (prog : Ast.prog) =
  if
    not
      (List.exists
         (function
           | Funcs f ->
               let _, fname = f.name in
               let _, dt = f.ret_type in
               fname = "main" && dt = Dint
           | Global_Vars _ -> false)
         prog)
  then failwith "Sivain: Error: Linker expects symbol main."

let print_err_usage_fail ?(usage = "") error_msg =
  prerr_endline error_msg;
  prerr_endline usage;
  exit 1

let check_filename_exe filename = Filename.extension filename = ".svn"
