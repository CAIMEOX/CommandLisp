let write_to_file (filename: string) (content: string): unit =
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" content;
  close_out oc
let objective (name: string): string = "scoreboard objectives add " ^ name ^ " dummy"
let link (dest: string) (obj: string) (value: int): string = "scoreboard players set " ^ dest ^ " " ^ obj ^ " " ^ (string_of_int value)
let operation_assign (dest: string) (src: string): string = "scoreboard players operation " ^ dest ^ " = " ^ src
(* let operation_add (dest: string) (src: string): string = "scoreboard players operation " ^ dest ^ " += " ^ src *)
let stack_init (size: int): string = 
  String.concat "\n" (List.init size (fun i ->
    objective ("stack_" ^ (string_of_int i)) ^ "\n" ^ link "stack" ("stack_" ^ (string_of_int i)) 0))

let register_init (label: string) (num: int): string =
  String.concat "\n" (List.init num (fun i ->
    objective (label ^ (string_of_int i)) ^ "\n" ^ link "cpu" (label ^ (string_of_int i)) 0))

let set_register (register: string) (value: int): string = 
  link "cpu" register value ^ "\n"
let cpu_init (): string = 
  register_init "x" 10 ^ "\n"
  ^ register_init "t" 10 ^ "\n"
  ^ register_init "a" 10 ^ "\n"
  ^ objective "pc" ^ "\n" ^ link "cpu" "pc" 0 ^ "\n"
  ^ objective "sp" ^ "\n" ^ link "cpu" "sp" 0 ^ "\n"

let stack_push (size: int) (register: string): string =
  write_to_file ("functions/stack_push_" ^ register ^ ".mcfunction") ((String.concat "\n" (List.init size (fun i ->
    "execute if score cpu sp matches " ^ (string_of_int i)  ^ " run " ^ operation_assign ("stack stack_" ^ (string_of_int i)) ("cpu " ^ register))))
  ^ "\n" ^ "scoreboard players add cpu sp 1\n");
  "function stack_push_" ^ register

let stack_pop (size: int) (register: string): string =
  write_to_file ("functions/stack_pop_" ^ register ^ ".mcfunction")
  ("scoreboard players add cpu sp -1\n" ^ (String.concat "\n" (List.init size (fun i ->
    "execute if score cpu sp matches " ^ (string_of_int i)  ^ " run " ^ operation_assign ("cpu " ^ register) ("stack stack_" ^ (string_of_int i)))))
  ^ "\n");
  "function stack_pop_" ^ register ^ "\n"

let prim_op (op: string) (register1: string) (register2: string): string =
  "scoreboard players operation cpu " ^ register1 ^ " " ^ op ^ " cpu " ^ register2 ^ "\n"