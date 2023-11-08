open Command

let () = print_endline (stack_init 10)
let () = print_endline (cpu_init ())
let () = print_endline (stack_push 10 "x0")
let () = print_endline (stack_pop 10 "x0")

(* write to file *)
let write_to_file (filename : string) (content : string) : unit =
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" content;
  close_out oc

let () =
  write_to_file "./functions/init.mcfunction"
    (stack_init 10 ^ "\n" ^ cpu_init ())

let instrs = Parse.compile (Add (Mul (Cst 114, Cst (-1)), Mul (Cst 514, Cst 2)))
let mcfn = Vm.compile instrs
let test_instr = Vm.print_instr instrs
let () = print_endline test_instr
let () = write_to_file "./functions/run.mcfunction" (String.concat "\n" mcfn)
