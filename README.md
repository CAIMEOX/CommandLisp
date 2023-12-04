# Command Lisp
![logo](raw/logo.png)

> The Project is WIP actively. Click to view the [design document](design.md)

Command Lisp is a simplified language designed for Minecraft Bedrock Command System, characterized by a very high level of abstraction, which is also a dialect of Lisp.

> What sets Lisp apart is its design to be self-evolving. In Lisp, you can define new Lisp operators. When new abstract concepts become popular (such as object-oriented programming), we always find that these new concepts are easiest to implement in Lisp. Lisp is like the DNA of languages, a language that will never become obsolete.

## Command Lisp Compiler (clc)
This repository is mainly the compiler of language Command Lisp (CLC). CLC is actively developing now but it still requires a lot of effort to accomplish the compiler.

### Set up environment
CL uses **dune** as the project manager like most ocaml projects. The project provides a quick environment for **nix**. The following command will automatically set up the developing environment:
```shell
nix-shell shell.nix
```

Or without nix:
```sh
sudo apt install ocaml opam
opam install core
```

### Build Project
```sh
dune build
```

### Run CLC
```sh
dune exec clc
```

## Why Command Lisp (CL)?

In Minecraft Bedrock, editing commands can sometimes be very challenging, and the unstable game code has caused us a lot of trouble. Moreover, many commands are not very intuitive, significantly reducing work efficiency. The birth of CL is aimed at addressing these pain points, and using it, you can experience the following benefits:

- Write once, run in multiple places: You only need to use CL to complete command writing, and you can use our tools to import multiple saves, achieving reusability.
- High abstraction, high efficiency: CL language allows you to achieve more possibilities with less code.
- Simple and easy to learn: Lisp language is very intuitive and easy to understand.
- Reduce errors: Compared to finding errors directly in the command system, finding errors in CL scripts is obviously simpler.

## Why Ocaml
This project tried many languages ​​during the development process, and finally chose Ocaml. Click [here](design.md#why-ocaml) to see the detail

## Basic Syntax
In command lisp we currently support int type.
```clojure
(let [n1 v1 n2 v2 ...] (expr))
(let [a 3 b 5] (+ a b)) ; 8
```

Define function and use it.
```clojure
(def name [param1 param2 param2 ...] (body) (expr))
(def square [n] (* n n) (square 10)) ; 100
```

Conditional
```clojure
(if (condition) (then) (else))
(if (> 4 3) 3 4) ; 3
(if (tag~ a "TEST") (write "Found player marked for TEST") (write "No player has the TEST tag"))
```

Inline Command (all these expression return `0` in default)
```clojure
(inline command) ; single
(seq [command1 command2 command3]) ; sequences
```

## Minecraft Commands
We provide some simple commands (Sugar)
```clojure
(tag+ @p TAG_1) ; tag @p add TAG_1
(tag- @r TAG_2) ; tag @p remove TAG_2
(if (tag~ @p TAG_3) (inline "say hello world")) ; execute if @p[tag=TAG_3] run say hello world
```

Write to screen
```clojure
(write "Hello World") ; [!]Hello World
```

Teleport

```clojure
(tp r r) ; "tp @r @r"
(tp a p) ; "tp @a @p"
```

## License
GNU GENERAL PUBLIC LICENSE Version 3

