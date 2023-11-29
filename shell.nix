with import <nixpkgs> {};

mkShell {
  buildInputs = [
    ocaml
  ] ++ (with ocamlPackages; [
    findlib
    ocamlformat
    utop
    core
  ]);

  shellHook = ''
    alias run="dune exec command_lisp"
    alias build="dune build @fmt --auto-promote"
    alias fmt="dune fmt"
    alias repl="dune utop"
  '';
}
