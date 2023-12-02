(lang dune 3.8)

(name command_lisp)

(generate_opam_files true)

(source
 (github CAIMEOX/CommandLisp))

(authors "CAIMEO")

(maintainers "CAIMEO")

(license GPL)

(documentation https://github.com/CAIMEOX/CommandLisp)

(package
 (name command_lisp)
 (synopsis "Command Lisp")
 (description "A tool that compiles lisp to mcfunction")
 (depends ocaml dune core))
