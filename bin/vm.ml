let max_mem : int = 128

type instr =
  | Pop of string
  | Swap of string * string
  | Add of string * string
  | Mul of string * string
  | Syscall
  | Goto of int
  | Cst of int
  | LoadMemory of string * int
  | SaveMemory of string * int
  | Exit

let compile_pop = Command.stack_pop max_mem

let compile_push v =
  Command.set_register "t0" v ^ Command.stack_push max_mem "t0"

let compile_swap r1 r2 =
  Command.stack_pop max_mem r1
  ^ Command.stack_pop max_mem r2
  ^ Command.stack_push max_mem r1
  ^ Command.stack_push max_mem r2

let compile_prim_op op r1 r2 =
  Command.stack_pop max_mem r1
  ^ Command.stack_pop max_mem r2
  ^ Command.prim_op op r1 r2
  ^ Command.stack_push max_mem r1

let compile_syscall = ""

let rec compile (instrs : instr list) : string list =
  match instrs with
  | [] -> []
  | instr :: instrs -> (
      match instr with
      | Pop r -> compile_pop r :: compile instrs (*Just pop*)
      | Swap (r1, r2) -> compile_swap r1 r2 :: compile instrs (*Swap*)
      | Add (r1, r2) ->
          compile_prim_op "+=" r1 r2 :: compile instrs (*Addition*)
      | Mul (r1, r2) ->
          compile_prim_op "*=" r1 r2 :: compile instrs (*Multiplication*)
      | Cst i -> compile_push i :: compile instrs (*Push constant*)
      | LoadMemory _ -> compile instrs (*Load memory, TODO*)
      | SaveMemory _ -> compile instrs (*Save Memory, TODO*)
      (* TO DO *)
      | Syscall -> "syscall" :: compile instrs
      | Goto i -> ("goto " ^ string_of_int i) :: compile instrs
      | Exit -> "exit" :: compile instrs)

let rec print_instr (instrs : instr list) : string =
  match instrs with
  | [] -> ""
  | instr :: instrs -> (
      match instr with
      | Pop r -> "Pop " ^ r ^ "\n" ^ print_instr instrs
      | Swap (r1, r2) -> "Swap " ^ r1 ^ " " ^ r2 ^ "\n" ^ print_instr instrs
      | Add (r1, r2) -> "Add " ^ r1 ^ " " ^ r2 ^ "\n" ^ print_instr instrs
      | Mul (r1, r2) -> "Mul " ^ r1 ^ " " ^ r2 ^ "\n" ^ print_instr instrs
      | Cst i -> "Cst " ^ string_of_int i ^ "\n" ^ print_instr instrs
      | LoadMemory _ -> "LoadMemory\n" ^ print_instr instrs
      | SaveMemory _ -> "SaveMemory\n" ^ print_instr instrs
      | Syscall -> "Syscall\n" ^ print_instr instrs
      | Goto i -> "Goto " ^ string_of_int i ^ "\n" ^ print_instr instrs
      | Exit -> "Exit\n" ^ print_instr instrs)
