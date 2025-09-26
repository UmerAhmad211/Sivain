open Ast

let check_for_main (prog : Ast.prog) =
  if
    not
      (List.exists
         (function
           | Funcs f ->
               let _, fname = f.name in
               fname = "main"
           | Global_Vars _ -> false)
         prog)
  then
    failwith "Sivain: Error: Linker expects symbol main."

let print_err_usage_fail ?(usage = "") error_msg =
  prerr_endline error_msg;
  prerr_endline usage;
  exit 1

let check_filename_exe filename = Filename.extension filename = ".svn"
