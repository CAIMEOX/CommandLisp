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
}
