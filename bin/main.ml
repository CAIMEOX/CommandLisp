open Command

(* write to file *)
let write_to_file (filename : string) (content : string) : unit =
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" content;
  close_out oc

let () =
  write_to_file "./functions/init.mcfunction"
    (stack_init 10 ^ "\n" ^ cpu_init ())

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
