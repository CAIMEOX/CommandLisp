type block_type = Chain | Repeat | Impulse

module McFunction = struct
  type mcfunction = { name : string; commands : string }

  let make_function : string -> string -> mcfunction =
   fun name commands -> { name; commands }

  type file_tree = { path : string; fn : mcfunction }

  let empty () = []

  let insert ft ftl =
    List.map (fun x -> if x.path = ft.path then ft else x) ftl

  (* won't create if do not exist *)
  let append ft ftl =
    List.map
      (fun x ->
        if x.path = ft.path then
          {
            x with
            fn = { x.fn with commands = x.fn.commands ^ "\n" ^ ft.fn.commands };
          }
        else x)
      ftl
end

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

(* The function trans system into mcs should generate the chain properly *)
