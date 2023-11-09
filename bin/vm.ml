let max_mem : int = 128
type label = string
type instr =
  | Cst of int
  | Pop of string
  | Swap of string * string
  | Add of string * string
  | Mul of string * string
  | Sub of string * string
  | Div of string * string
  | Call of label * int
  | Var of int
  | Ret of int
  | Label of label
  | IfZero of label
  | Goto of label
  | LoadMemory of string * int
  | SaveMemory of string * int
  | Syscall
  | Exit

let get_label_index (instr : instr array) (label: label) : int =
  let rec aux i =
    if i >= Array.length instr then failwith "Label not found"
    else if instr.(i) = Label label then i
    else aux (i + 1)
  in aux 0

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

let encode (instrs : instr array) : string list =
  Array.map (function
  | Pop r -> compile_pop r
  | Swap (r1, r2) -> compile_swap r1 r2
  | Add (r1, r2) -> compile_prim_op "+=" r1 r2
  | Mul (r1, r2) -> compile_prim_op "*=" r1 r2
  | Sub (r1, r2) -> compile_prim_op "-=" r1 r2
  | Div (r1, r2) -> compile_prim_op "/=" r1 r2
  | Cst i -> compile_push i
  | Label l -> "# " ^ l ^ ":"
  | Var n -> compile_push n
  | Call (_) -> "call"
  | Ret _ -> "ret"
  | IfZero _ -> "if0"
  | Goto i -> get_label_index instrs i |> Command.set_register "pc" 
  | Exit -> "function init"
  | Syscall -> "syscall"
  | LoadMemory _ -> "ld"
  | SaveMemory _ -> "sd"
  ) instrs |> Array.to_list

let rec print_instr (instrs : instr list) : string =
  match instrs with
  | [] -> ""
  | instr :: instrs -> (
      match instr with
      | Pop _ -> "Pop\n" ^ print_instr instrs
      | Swap (_) -> "Swap\n" ^ print_instr instrs
      | Add (_) -> "Add\n" ^ print_instr instrs
      | Mul (_) -> "Mul\n" ^ print_instr instrs
      | Sub (_) -> "Sub\n" ^ print_instr instrs
      | Div (_) -> "Div\n" ^ print_instr instrs
      | Call (label, n) -> "Call " ^ label ^ " " ^ string_of_int n ^ "\n" ^ print_instr instrs
      | Ret n -> "Ret " ^ string_of_int n ^ "\n" ^ print_instr instrs
      | IfZero r -> "IfZero " ^ r ^ "\n" ^ print_instr instrs
      | Var n -> "Var " ^ string_of_int n ^ "\n" ^ print_instr instrs
      | Cst i -> "Cst " ^ string_of_int i ^ "\n" ^ print_instr instrs
      | Label l -> "Label " ^ l ^ ":\n" ^ print_instr instrs
      | LoadMemory _ -> "LoadMemory\n" ^ print_instr instrs
      | SaveMemory _ -> "SaveMemory\n" ^ print_instr instrs
      | Syscall -> "Syscall\n" ^ print_instr instrs
      | Goto i -> "Goto " ^ i ^ "\n" ^ print_instr instrs
      | Exit -> "Exit\n" ^ print_instr instrs)
