let max_mem : int = 128
let ret_stack_size = ref 0;
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

let get_label_index (instr : instr array) (label : label) : int =
  let rec aux i =
    if i >= Array.length instr then failwith "Label not found"
    else if instr.(i) = Label label then i
    else aux (i + 1)
  in
  aux 0

open Command

let ( >> ) = Util.( >> )
let compile_pop = StackFrame.pop max_mem

let compile_if0 (t : label) =
  compile_pop "x0"
  >> Execute.execute
       (Execute.cond_score_eq "cpu x0" "cpu a0")
       (StackFrame.run_function t)

let compile_push v = Register.reg_set "t0" v >> StackFrame.push max_mem "t0"

let compile_swap r1 r2 =
  StackFrame.pop max_mem r1 >> StackFrame.pop max_mem r2 >> StackFrame.push max_mem r1
  >> StackFrame.push max_mem r2

let compile_prim_op op r1 r2 =
  StackFrame.pop max_mem r1 >> StackFrame.pop max_mem r2 >> Register.reg_op op r1 r2
  >> StackFrame.push max_mem r1

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
  let (i, label) = next () in 
   let _ = RetStack.link "t0" i label in 
    "function " ^ f >>
    RetStack.push !ret_stack_size "t0" >>
    "# " ^ label

let compile_syscall = ""

let compute_ret_stack_size (instrs : instr array) : int =
  let rec aux i acc =
    if i >= Array.length instrs then acc
    else
      match instrs.(i) with
      | Call _ -> aux (i + 1) (acc + 1)
      | _ -> aux (i + 1) acc
  in aux 0 0

let encode (instrs : instr array) : string list =
  ret_stack_size := compute_ret_stack_size instrs;
  Array.map
    (function
      | Pop r -> compile_pop r
      | Swap (r1, r2) -> compile_swap r1 r2
      | Add (r1, r2) -> compile_prim_op "+=" r1 r2
      | Mul (r1, r2) -> compile_prim_op "*=" r1 r2
      | Sub (r1, r2) -> compile_prim_op "-=" r1 r2
      | Div (r1, r2) -> compile_prim_op "/=" r1 r2
      | Cst i -> compile_push i
      | Label l -> "# " ^ l ^ ":"
      | Var n -> compile_var n
      | Call (f, _) -> compile_call f
      | Ret n -> compile_ret n
      | IfZero t -> compile_if0 t
      | Goto i -> "function " ^ i
      | Exit -> "function init"
      | Syscall -> "syscall"
      | LoadMemory _ -> "ld"
      | SaveMemory _ -> "sd")
    instrs
  |> Array.to_list

let rec print_instr (instrs : instr list) : string =
  match instrs with
  | [] -> ""
  | instr :: instrs -> (
      match instr with
      | Pop _ -> "Pop\n" ^ print_instr instrs
      | Swap _ -> "Swap\n" ^ print_instr instrs
      | Add _ -> "Add\n" ^ print_instr instrs
      | Mul _ -> "Mul\n" ^ print_instr instrs
      | Sub _ -> "Sub\n" ^ print_instr instrs
      | Div _ -> "Div\n" ^ print_instr instrs
      | Call (label, n) ->
          "Call " ^ label ^ " " ^ string_of_int n ^ "\n" ^ print_instr instrs
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

let split_tags (commands: string) : (string * string) list = 
  let lines = String.split_on_char '\n' commands in
    let result = ref [] in
    let cur_title = ref "# entry:" in
      for i = 0 to List.length lines - 1 do 
        let line = List.nth lines i in 
          if String.length line > 0 && line.[0] = '#' then 
            cur_title := line
          else 
            result := (!cur_title, line) :: !result
      done;
    !result
  (* split the string with the line starts with # *)
  
let format_tag tag = 
  let tag = String.sub tag 2 (String.length tag - 2) in 
    let tag = String.split_on_char ':' tag in 
      List.nth tag 0

let file_name= Printf.sprintf "functions/%s.mcfunction" 

let save_all_funs (fn_list: (string * string) list) = 
  let cur_title = ref (fst (List.hd fn_list)) in
  let content = ref "" in
  fn_list |> List.iter (fun (title, fn) -> 
    print_endline !content;
    if title <> !cur_title then
      let tag = format_tag !cur_title in 
      (Util.write_to_file (file_name tag) !content;
      cur_title := title;
      content := "function " ^ tag)
    else 
      content := fn >> !content);