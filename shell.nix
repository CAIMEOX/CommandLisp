with import <nixpkgs> {};

mkShell {
  buildInputs = [
    ocaml
  ] ++ (with ocamlPackages; [
    findlib
    ocamlformat
    utop
    core
    alcotest
    yojson
  ]);

  shellHook = ''
    alias run="dune exec clc"
    alias clc="dune exec clc"
    alias build="dune build @fmt --auto-promote"
    alias watch="dune build @fmt --auto-promote -w"
    alias fmt="dune fmt"
    alias repl="dune utop"
    alias test="dune runtest --auto-promote"
  '';
}
