let max_mem : int = 128
let ret_stack_size = ref 0

type label = string

type instr =
  | Cst of int
  | Pop
  | Swap
  | Add
  | Mul
  | Sub
  | Div
  | Call of label * int
  | Var of int
  | Ret of int
  | Label of label
  | IfZero of label
  | Goto of label
  | Syscall
  | Exit

let string_of_instr instr =
  match instr with
  | Cst i -> "Cst " ^ string_of_int i
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Var i -> "Var " ^ string_of_int i
  | Pop -> "Pop"
  | Swap -> "Swap"
  | Label l -> "Label " ^ l
  | Ret n -> "Ret " ^ string_of_int n
  | Call (f, n) -> "Call " ^ f ^ " " ^ string_of_int n
  | IfZero l -> "IfZero " ^ l
  | Goto l -> "Goto " ^ l
  | Exit -> "Exit"
  | Syscall -> "Syscall"

let get_label_index (instr : instr array) (label : label) : int =
  let rec aux i =
    if i >= Array.length instr then failwith "Label not found"
    else if instr.(i) = Label label then i
    else aux (i + 1)
  in
  aux 0

open Command
open Control

let ( >> ) = Util.( >> )
let compile_pop = StackFrame.pop max_mem

let compile_if0 (t : label) =
  compile_pop "x0"
  >> Execute.execute
       (Execute.cond_score_eq "cpu x0" "cpu a0")
       (StackFrame.run_function t)

let compile_push v = Register.reg_set "t0" v >> StackFrame.push max_mem "t0"

let compile_swap r1 r2 =
  StackFrame.pop max_mem r1 >> StackFrame.pop max_mem r2
  >> StackFrame.push max_mem r1 >> StackFrame.push max_mem r2

let compile_prim_op op r1 r2 =
  StackFrame.pop max_mem r1 >> StackFrame.pop max_mem r2
  >> Register.reg_op op r1 r2 >> StackFrame.push max_mem r1

(*
  | x0 <- pop 
  | sp <- sp - n 
  | Ret.pop t1
  | push stack t0
  | goto t1
  *)
let compile_ret n =
  compile_pop "t0" (* res *)
  >> ScoreBoard.unary_sub "cpu sp" n
  >> RetStack.pop !ret_stack_size "t1"
  >> StackFrame.push max_mem "t0"
  >> "function ret_stack_control_t0"

(*
  | cpu t0 <- cpu sp
  | cpu t0 -= n
  | cpu t1 <- StackFrame.cpu t0]
  | push cpu t1
  *)
let compile_var n =
  ScoreBoard.binop_assign "cpu t0" "cpu sp"
  >> ScoreBoard.unary_sub "cpu t0" (n + 1)
  >> StackFrame.access max_mem "t1" "t0"
  >> StackFrame.push max_mem "t1"

let call_index = ref 0

let next () =
  let n = !call_index in
  call_index := n + 1;
  (n, "fn_call_" ^ string_of_int n)

let compile_call f =
  let i, label = next () in
  let _ = RetStack.link "t0" i label in
  "function " ^ f >> RetStack.push !ret_stack_size "t0" >> "# " ^ label

let compile_syscall = ""

let compute_ret_stack_size (instrs : instr array) : int =
  let rec aux i acc =
    if i >= Array.length instrs then acc
    else
      match instrs.(i) with
      | Call _ -> aux (i + 1) (acc + 1)
      | _ -> aux (i + 1) acc
  in
  aux 0 0

let encode (instrs : instr array) : string list =
  ret_stack_size := compute_ret_stack_size instrs;
  Array.map
    (function
      (* TODO: assign registers by module Control *)
      | Pop -> compile_pop "a0"
      | Swap -> compile_swap "a0" "a1"
      | Add -> compile_prim_op "+=" "a0" "a1"
      | Mul -> compile_prim_op "*=" "a0" "a1"
      | Sub -> compile_prim_op "-=" "a0" "a1"
      | Div -> compile_prim_op "/=" "a0" "a1"
      | Cst i -> compile_push i
      | Label l -> "# " ^ l ^ ":"
      | Var n -> compile_var n
      | Call (f, _) -> compile_call f
      | Ret n -> compile_ret n
      | IfZero t -> compile_if0 t
      | Goto i -> "function " ^ i
      | Exit -> "function init"
      | Syscall -> "syscall")
    instrs
  |> Array.to_list

let rec print_instr (instrs : instr list) : string =
  match instrs with
  | [] -> ""
  | instr :: instrs -> (
      match instr with
      | Pop -> "Pop\n" ^ print_instr instrs
      | Swap -> "Swap\n" ^ print_instr instrs
      | Add -> "Add\n" ^ print_instr instrs
      | Mul -> "Mul\n" ^ print_instr instrs
      | Sub -> "Sub\n" ^ print_instr instrs
      | Div -> "Div\n" ^ print_instr instrs
      | Call (label, n) ->
          "Call " ^ label ^ " " ^ string_of_int n ^ "\n" ^ print_instr instrs
      | Ret n -> "Ret " ^ string_of_int n ^ "\n" ^ print_instr instrs
      | IfZero r -> "IfZero " ^ r ^ "\n" ^ print_instr instrs
      | Var n -> "Var " ^ string_of_int n ^ "\n" ^ print_instr instrs
      | Cst i -> "Cst " ^ string_of_int i ^ "\n" ^ print_instr instrs
      | Label l -> "Label " ^ l ^ ":\n" ^ print_instr instrs
      | Syscall -> "Syscall\n" ^ print_instr instrs
      | Goto i -> "Goto " ^ i ^ "\n" ^ print_instr instrs
      | Exit -> "Exit\n" ^ print_instr instrs)

let split_tags (commands : string) : (string * string) list =
  let lines = String.split_on_char '\n' commands in
  let result = ref [] in
  let cur_title = ref "# entry:" in
  for i = 0 to List.length lines - 1 do
    let line = List.nth lines i in
    if String.length line > 0 && line.[0] = '#' then cur_title := line
    else result := (!cur_title, line) :: !result
  done;
  !result
(* split the string with the line starts with # *)

let format_tag tag =
  let tag = String.sub tag 2 (String.length tag - 2) in
  let tag = String.split_on_char ':' tag in
  List.nth tag 0

let file_name = Printf.sprintf "functions/%s.mcfunction"

let save_all_funs (fn_list : (string * string) list) =
  let cur_title = ref (fst (List.hd fn_list)) in
  let content = ref "" in
  fn_list
  |> List.iter (fun (title, fn) ->
         if title <> !cur_title then (
           let tag = format_tag !cur_title in
           Util.write_to_file (file_name tag) !content;
           cur_title := title;
           content := "function " ^ tag)
         else content := fn >> !content)

type block_type = Chain | Repeat | Impulse

type command_block = {
  command : string;
  block_type : block_type;
  condition : bool;
  auto : bool;
  execute_on_first_tick : bool;
  hover_note : string;
  delay : int;
  previous_output : string;
}

type command_sequence = command_block list

type system = {
  name : string;
  init_program : command_sequence;
  entry_program : command_sequence;
  programs : command_sequence list;
  boundbox : int * int * int; (* max size of system *)
}

let system_init_with_size x y z =
  {
    name = "cl";
    init_program = [];
    entry_program = [];
    programs = [];
    boundbox = (x, y, z);
  }
