open Command

module Register = struct
  let free_x_registers = ref (Array.make 10 true)

  let is_free_x_register i =
    if !free_x_registers.(i) then (
      !free_x_registers.(i) <- false;
      true)
    else false

  let get_x_register () =
    let rec get_x_register_helper i =
      if i = 10 then failwith "No free x registers"
      else if is_free_x_register i then Printf.sprintf "x%d" i
      else get_x_register_helper (i + 1)
    in
    get_x_register_helper 0
end

module EntityStack = struct
  open Position
  open TargetSelector
  open Execute
  open ScoreBoard

  type namespace = { top : string; value : string; tmp : string }

  (* entity may serve as data stack or ret stack *)
  type entity_stack = {
    ns : namespace;
    entity_type : string;
    matrix_size : int;
    direction : direction;
  }

  let new_data_stack entity_type matrix_size direction =
    {
      ns = { top = "data_stack_top"; value = "data_stack_value"; tmp = "data_stack_tmp" };
      entity_type;
      direction;
      matrix_size;
    }

  let new_call_stack entity_type matrix_size direction =
    {
      ns = { top = "call_stack_top"; value = "call_stack_value"; tmp = "call_stack_tmp" };
      entity_type;
      direction;
      matrix_size;
    }

  let ( -- ) i j = List.init (j - i + 1) (fun x -> x + i)
  let linearize_command seq = String.concat "\n" seq

  (* The fp points to the current running function *)
  let cpu_init =
    [
      Objective (Add ("data_stack_value", "")) |> string_of_scoreboard;
      Objective (Add ("call_stack_value", "")) |> string_of_scoreboard;
      Objective (Add ("fp", "")) |> string_of_scoreboard;
      Player (Set (Name "cpu", "fp", -1)) |> string_of_scoreboard;
    ]

  let registers_init x =
    List.concat_map
      (fun i ->
        [
          Objective (Add (x ^ string_of_int i, "")) |> string_of_scoreboard;
          Player (Set (Name "cpu", x ^ string_of_int i, 0)) |> string_of_scoreboard;
        ])
      (0 -- 10)

  let summon_relative sel name d offset =
    let open Summon in
    Execute
      [
        Modify (At sel);
        Modify (Positioned (Relative (0, 1, 0)));
        Run (Summon (name, direction_to_position d offset) |> string_of_summon);
      ]
    |> string_of_execute

  let tag_entity_stack sel es =
    Execute
      [
        Modify (At sel);
        Modify (Positioned (Relative (0, 1, 0)));
        Modify
          (As
             (VarArg
                (AllEntities, [ get_offset_dire es.direction es.matrix_size; Type es.entity_type ])));
        Run (Player (Set (Var Self, es.ns.value, 0)) |> string_of_scoreboard);
      ]
    |> string_of_execute

  let set_header sel es =
    Execute
      [
        Modify (At sel);
        Modify (Positioned (Relative (0, 1, 0) ++ direction_to_position es.direction 1));
        Modify (As (VarArg (AllEntities, [ R 0.5; Type es.entity_type ])));
        Run (Tag.Add (Var Self, es.ns.top) |> Tag.string_of_tag);
      ]
    |> string_of_execute

  let generate_entity es =
    let spawn = summon_relative (Var Self) es.entity_type es.direction in
    let stack_header = set_header (Var Self) es in
    let tag_all = tag_entity_stack (Var Self) es in
    List.map spawn (1 -- es.matrix_size) @ [ tag_all; stack_header ]

  let stack_top_ref es = VarArg (AllEntities, [ Type es.entity_type; Tag es.ns.top ])
  let compile_store reg value = Player (Set (Name "cpu", reg, value)) |> string_of_scoreboard

  let compile_load_to es reg =
    Player (Operation (Name "cpu", reg, Set, Var Self, es.ns.value)) |> string_of_scoreboard

  let compile_load_from es reg =
    Player (Operation (Var Self, es.ns.value, Set, Name "cpu", reg)) |> string_of_scoreboard

  let compile_fun_call name =
    let open Function in
    string_of_function name

  let compile_switch name =
    Player (Set (Name "cpu", "fp", int_of_string name)) |> string_of_scoreboard

  let compile_runtime =
    List.map (fun i ->
        Execute
          [
            Option (If (Score (MatchesValue (Name "cpu", "fp", int_of_string i))));
            Run (compile_fun_call i);
          ]
        |> string_of_execute)

  let compile_if_then reg v cmd =
    let open Execute in
    Execute [ Option (If (Score (MatchesValue (Name "cpu", reg, v)))); Run cmd ]
    |> string_of_execute

  (* peek offset sp - n and load to reg *)
  let compile_peek es reg n =
    Execute
      [
        Modify (PositionedAs (stack_top_ref es));
        Run
          (Execute
             [
               Modify (Positioned (direction_to_position es.direction (-n)));
               Modify (As (VarArg (AllEntities, [ R 0.5; Type es.entity_type ])));
               Run (compile_load_to es reg);
             ]
          |> string_of_execute);
      ]
    |> string_of_execute

  let compile_move_sp es n =
    [
      Tag.Add (stack_top_ref es, es.ns.tmp) |> Tag.string_of_tag;
      Execute
        [
          Modify (PositionedAs (VarArg (AllEntities, [ Tag es.ns.tmp ])));
          Run
            (Execute
               [
                 Modify (Positioned (direction_to_position es.direction n));
                 Modify (As (VarArg (AllEntities, [ R 0.5; Type es.entity_type ])));
                 Run (Tag.Add (Var Self, es.ns.top) |> Tag.string_of_tag);
               ]
            |> string_of_execute);
        ]
      |> string_of_execute;
      Tag.Remove (VarArg (AllEntities, [ Type es.entity_type; Tag es.ns.tmp ]), es.ns.top)
      |> Tag.string_of_tag;
      Tag.Remove (VarArg (AllEntities, [ Type es.entity_type; Tag es.ns.tmp ]), es.ns.tmp)
      |> Tag.string_of_tag;
    ]

  let compile_pop es reg =
    (Execute
       [
         Modify (As (VarArg (AllEntities, [ Type es.entity_type; Tag es.ns.top ])));
         Run (compile_load_to es reg);
       ]
    |> string_of_execute)
    :: compile_move_sp es (-1)

  let compile_push es reg =
    compile_move_sp es 1
    @ [
        Execute
          [
            Modify (As (VarArg (AllEntities, [ Type es.entity_type; Tag es.ns.top ])));
            Run (compile_load_from es reg);
          ]
        |> string_of_execute;
      ]

  let compile_prim typ reg1 reg2 =
    [ Player (Operation (Name "cpu", reg1, typ, Name "cpu", reg2)) |> string_of_scoreboard ]
end
