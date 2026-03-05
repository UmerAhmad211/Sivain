open Util

let run_comm comm args =
  let pid = Unix.create_process comm args Unix.stdin Unix.stderr Unix.stdout in
  let _, status = Unix.waitpid [] pid in
  match status with Unix.WEXITED 0 -> () | _ -> failwith (comm ^ "failed.")

let build_dir = ".siv-build"

let driver past target cc file output backend =
  let cgened =
    match backend with
    | Qbe -> Qbe_gen.emitter_driver past
    | Llvm -> Llvm_gen.emitter_driver past
  in

  let file_exe = if backend = Qbe then ".ssa" else ".ll" in
  if not (Sys.file_exists build_dir) then Unix.mkdir build_dir 0o755;
  let ssa_fname =
    (build_dir ^ "/" ^ (file |> Filename.basename |> Filename.remove_extension))
    ^ file_exe
  in
  let oc = open_out ssa_fname in
  output_string oc cgened;
  flush oc;
  close_out oc;

  if backend = Qbe then (
    let asm_fname = build_dir ^ "/" ^ output ^ ".s" in
    run_comm "qbe"
      [| "qbe"; ssa_fname; "-t"; string_of_target_qbe target; "-o"; asm_fname |];
    run_comm cc [| cc; asm_fname; "-static"; "-o"; output |])
  else
    run_comm "clang"
      [|
        "clang";
        ssa_fname;
        "-static";
        string_of_target_llvm target;
        "-o";
        output;
      |]
