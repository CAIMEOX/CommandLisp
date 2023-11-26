# Command Lisp
> The Project is WIP actively. Click to view the [design document](design.md)

Command Lisp is a simplified language designed for Minecraft Bedrock, characterized by a very high level of abstraction. At the same time, it is also a dialect of Lisp.

> What sets Lisp apart is its design to be self-evolving. In Lisp, you can define new Lisp operators. When new abstract concepts become popular (such as object-oriented programming), we always find that these new concepts are easiest to implement in Lisp. Lisp is like the DNA of languages, a language that will never become obsolete.

### Why Command Lisp (CL)?

In Minecraft Bedrock, editing commands can sometimes be very challenging, and the unstable game code has caused us a lot of trouble. Moreover, many commands are not very intuitive, significantly reducing work efficiency. The birth of CL is aimed at addressing these pain points, and using it, you can experience the following benefits:

- Write once, run in multiple places: You only need to use CL to complete command writing, and you can use our tools to import multiple saves, achieving reusability.
- High abstraction, high efficiency: CL language allows you to achieve more possibilities with less code.
- Simple and easy to learn: Lisp language is very intuitive and easy to understand.
- Reduce errors: Compared to finding errors directly in the command system, finding errors in CL scripts is obviously simpler.

## Basic Syntax
In command lisp we currently support int type.
```clojure
(let [n1 v1 n2 v2 ...] (expr))
(let [a 3 b 5] (+ a b)) ; 8
```

Define function and use
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
GPL V3

