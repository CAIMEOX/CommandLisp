open Parse
let fact (e: expr) = Letfn (
  "fact",
  ["n"],
  If(
    Var("n"),
    Prim(Mul, [Var("n"); Prim(Self, [Prim(Add,  [Var("n"); Cst(-1)])])]),
    Cst(1)
  ),
  e
)

let p_fact = preprocess_and_compile (fact (Cst 3))
let encoded = Array.of_list p_fact |> Vm.encode
let () = List.iter (fun i -> print_endline i) encoded
let () = print_endline (Vm.print_instr p_fact)

open Command

(* let () = print_endline ((Stack.init 30) |> Util.flatten)
let () = print_endline ((Register.init) |> Util.flatten)
let () = print_endline ((Stack.generate_pop 32 "x0") |> Util.flatten)
let () = print_endline ((Stack.generate_push 32 "x0") |> Util.flatten)
let () = Stack.push 32 "x0" |> print_endline
let () = Stack.pop 100 "x0" |> print_endline *)

let () = Stack.access 32 "cpu a0" "cpu x0" |> print_endline