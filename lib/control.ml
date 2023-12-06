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
      else if is_free_x_register i then Printf.sprintf "%d" i
      else get_x_register_helper (i + 1)
    in
    get_x_register_helper 0
end

module EntityStack = struct
  open Position

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
      ns =
        {
          top = "data_stack_top";
          value = "data_stack_value";
          tmp = "data_stack_tmp";
        };
      entity_type;
      direction;
      matrix_size;
    }

  let new_call_stack entity_type matrix_size direction =
    {
      ns =
        {
          top = "call_stack_top";
          value = "call_stack_value";
          tmp = "call_stack_tmp";
        };
      entity_type;
      direction;
      matrix_size;
    }

  let ( -- ) i j = List.init (j - i + 1) (fun x -> x + i)

  open TargetSelector
  open Execute
  open ScoreBoard

  let linearize_command seq = String.concat "\n" seq

  let summon_relative sel name d offset =
    let open Summon in
    Execute
      [
        Modify (At sel);
        Run (Summon (name, direction_to_position d offset) |> string_of_summon);
      ]
    |> string_of_execute

  let generate_entity es =
    let spawn = summon_relative (Var Self) es.entity_type es.direction in
    List.map spawn (1 -- es.matrix_size)

  let stack_top_ref es =
    VarArg (AllEntities, [ Type es.entity_type; Tag es.ns.top; Count 1 ])

  let compile_store reg value =
    Player (Set (Name "cpu", reg, value)) |> string_of_scoreboard

  let compile_load_to es reg =
    Player (Operation (Name "cpu", reg, Set, Var Self, es.ns.value))
    |> string_of_scoreboard

  let compile_load_from es reg =
    Player (Operation (Var Self, es.ns.value, Set, Name "cpu", reg))
    |> string_of_scoreboard

  let compile_fun_call name =
    let open Function in
    string_of_function name

  let compile_if_then reg v cmd =
    let open Execute in
    Execute [ Option (If (Score (MatchesValue (Name "cpu", reg, v)))); Run cmd ]
    |> string_of_execute

  (* peek offset sp - n and load to reg *)
  let compile_peek es reg n =
    Execute
      [
        Modify (PositionedAs (stack_top_ref es));
        Modify (Positioned (direction_to_position es.direction (-n)));
        Modify (As (VarArg (AllEntities, [ R 0.5 ])));
        Run (compile_load_to es reg);
      ]
    |> string_of_execute

  let compile_move_sp es n =
    [
      Tag.Add (stack_top_ref es, es.ns.tmp) |> Tag.string_of_tag;
      Execute
        [
          Modify (As (VarArg (AllEntities, [ Tag es.ns.tmp ])));
          Modify (Positioned (direction_to_position es.direction n));
          Modify (As (VarArg (AllEntities, [ R 0.5 ])));
          Run (Tag.Add (Var Self, es.ns.top) |> Tag.string_of_tag);
        ]
      |> string_of_execute;
      Tag.Remove
        (VarArg (AllEntities, [ Type es.entity_type; Tag es.ns.tmp ]), es.ns.top)
      |> Tag.string_of_tag;
      Tag.Remove
        (VarArg (AllEntities, [ Type es.entity_type; Tag es.ns.tmp ]), es.ns.tmp)
      |> Tag.string_of_tag;
    ]

  let compile_pop es reg =
    (Execute
       [
         Modify
           (As (VarArg (AllEntities, [ Type es.entity_type; Tag es.ns.top ])));
         Run (compile_load_to es reg);
       ]
    |> string_of_execute)
    :: compile_move_sp es (-1)

  let compile_push es reg =
    compile_move_sp es 1
    @ [
        Execute
          [
            Modify
              (As (VarArg (AllEntities, [ Type es.entity_type; Tag es.ns.top ])));
            Run (compile_load_from es reg);
          ]
        |> string_of_execute;
      ]

  let compile_prim typ reg1 reg2 =
    [
      Player (Operation (Name "cpu", reg1, typ, Name "cpu", reg2))
      |> string_of_scoreboard;
    ]
end
