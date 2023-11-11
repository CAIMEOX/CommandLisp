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
let ccd = String.concat "\n" encoded
(* let () = print_endline ccd *)
let removed_tags = Vm.split_tags ccd
let () = List.iter (fun (x,y) -> print_endline (x ^ " " ^ y)) (List.rev removed_tags)
let () = Vm.save_all_funs removed_tags