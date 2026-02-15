# Sivian 

An experimental compiler that uses QBE as the compiler backend.
Sivian can cross compiler the code to amd64_sysv, arm64 and riscv64.

# Build

Must have QBE installed and in path.
Install Cmdliner using the command ```opam install cmdliner```.
Type ```dune build```.
View manpage by typing ```dune exec -- src/svc.exe --help```. Follow the instructions in the manpage to compile the code.
Compiler creates a hidden directory by the name of ```.siv-build``` which contains temporary files emitted during the compiler pipeline.
