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
  { name = "cl"; init_program = []; entry_program = []; programs = []; boundbox = (x, y, z) }

type file_list = string * string list

let file_add lst name content = (name, content) :: lst

let file_save lst prefix =
  let open Util in
  List.iter
    (fun (name, content) ->
      write_to_file (Printf.sprintf "../../../%s/%s.mcfunction" prefix name) content)
    lst

open Compile
open Control

type reg_collection = { tmp : string; x : string; y : string; fp : string }

let new_register_collection () =
  {
    tmp = Register.get_x_register ();
    x = Register.get_x_register ();
    y = Register.get_x_register ();
    fp = "fp";
  }

let compile_single rc es cs instr =
  let open Casm in
  let open Command in
  let open EntityStack in
  match instr with
  | Add ->
      compile_pop es rc.x @ compile_pop es rc.y
      @ compile_prim ScoreBoard.Add rc.x rc.y
      @ compile_push es rc.x
  | Sub ->
      compile_pop es rc.x @ compile_pop es rc.y
      @ compile_prim ScoreBoard.Sub rc.x rc.y
      @ compile_push es rc.x
  | Mul ->
      compile_pop es rc.x @ compile_pop es rc.y
      @ compile_prim ScoreBoard.Mul rc.x rc.y
      @ compile_push es rc.x
  | Div ->
      compile_pop es rc.x @ compile_pop es rc.y
      @ compile_prim ScoreBoard.Div rc.x rc.y
      @ compile_push es rc.x
  | Cst i -> [ compile_store rc.x i ] @ compile_push es rc.x
  | Pop -> compile_pop es rc.tmp
  | Swap -> compile_pop es rc.x @ compile_pop es rc.y @ compile_push es rc.x @ compile_push es rc.y
  | Exit -> compile_pop es rc.x @ [ compile_store rc.fp (-1) ]
  | Ret n ->
      compile_pop es rc.x @ compile_move_sp es (-n) @ compile_push es rc.x @ compile_pop cs rc.fp
  | Call (f, n) ->
      (compile_store rc.x n :: compile_push cs rc.x) @ [ compile_store rc.fp (int_of_string f) ]
  | Label x | Goto x -> [ compile_fun_call x ]
  | Var i -> [ compile_peek es rc.x i ] @ compile_push es rc.x
  | _ -> failwith ""

let compile_fntbale ft =
  let open Casm in
  let open EntityStack in
  let rc = new_register_collection () in
  let es = EntityStack.new_data_stack "armor_stand" 50 X in
  let cs = EntityStack.new_call_stack "armor_stand" 50 X in
  let casm_to_command instrs =
    let rec aux set res =
      match set with
      | IfZero t :: Goto l :: instrs ->
          let goto_l = compile_store rc.fp (int_of_string l) in
          let ins = compile_pop es rc.x @ [ compile_if_then rc.x 0 (compile_switch t) ] in
          aux instrs (res @ (goto_l :: ins))
      | x :: instrs -> aux instrs (res @ compile_single rc es cs x)
      | [] -> res
    in
    aux instrs []
  in
  List.map (fun (name, instrs) -> (name, instrs |> casm_to_command |> String.concat "\n")) ft
  @ [
      ("init_data", generate_entity es |> linearize_command);
      ("init_call", generate_entity cs |> linearize_command);
      ( "init",
        registers_init "t" @ registers_init "x" @ registers_init "a" @ cpu_init |> linearize_command
      );
      ("ticking", compile_runtime (List.map fst ft) |> linearize_command);
      ("debug_pop", compile_pop es rc.x |> linearize_command);
      ("debug_push", compile_store rc.x 114 :: compile_push es rc.x |> linearize_command);
    ]
