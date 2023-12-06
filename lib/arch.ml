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

open Compile
open Control

type reg_collection = { tmp : string; x : string; y : string }

let new_register_collection () =
  {
    tmp = Register.get_x_register ();
    x = Register.get_x_register ();
    y = Register.get_x_register ();
  }

let compile_single rc es _cs instr =
  let open Casm in
  let open Command in
  let open EntityStack in
  match instr with
  | Add ->
      compile_pop es rc.x @ compile_pop es rc.y
      @ compile_prim ScoreBoard.Add rc.x rc.y
  | Sub ->
      compile_pop es rc.x @ compile_pop es rc.y
      @ compile_prim ScoreBoard.Sub rc.x rc.y
  | Mul ->
      compile_pop es rc.x @ compile_pop es rc.y
      @ compile_prim ScoreBoard.Mul rc.x rc.y
  | Div ->
      compile_pop es rc.x @ compile_pop es rc.y
      @ compile_prim ScoreBoard.Div rc.x rc.y
  | Cst i -> [ compile_store rc.x i ] @ compile_push es rc.x
  | Pop -> compile_pop es rc.tmp
  | Swap ->
      compile_pop es rc.x @ compile_pop es rc.y @ compile_push es rc.x
      @ compile_push es rc.y
  | Exit -> compile_pop es rc.x
  | Ret n ->
      compile_pop es rc.x @ compile_move_sp es (-n)
      @ compile_push es rc.x (* to do *)
  | IfZero t ->
      compile_pop es rc.x @ [ compile_if_then rc.x 0 (compile_fun_call t) ]
  | Call _ -> []
  | Label x | Goto x -> [ compile_fun_call x ]
  | Var i -> [ compile_peek es rc.x i ] @ compile_push es rc.x
  | _ -> failwith ""

let casm_to_command instrs =
  let rc = new_register_collection () in
  let es = EntityStack.new_data_stack "wither_skull" 10 Y in
  let cs = EntityStack.new_call_stack "wither_skull" 10 Y in
  let rec aux instrs rest =
    match instrs with [] -> rest | i :: r -> aux r (compile_single rc es cs i)
  in
  aux instrs []

let function_table_compile table =
  List.map (fun (name, instrs) -> (name, casm_to_command instrs)) table
