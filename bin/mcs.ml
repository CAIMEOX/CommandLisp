type block_type = Chain | Repeat | Impulse

module McFunction = struct
  type mcfunction = { name : string; commands : string }

  let make_function : string -> string -> mcfunction =
   fun name commands -> { name; commands }

  type fileTree = File of string * string | Folder of string * fileTree list

  let pretty_print : fileTree -> string =
   fun tree ->
    let rec aux : fileTree -> string -> string =
     fun tree indent ->
      match tree with
      | File (name, _) -> indent ^ name
      | Folder (name, trees) ->
          indent ^ name ^ "\n"
          ^ String.concat "\n" (List.map (fun t -> aux t (indent ^ "  ")) trees)
    in
    aux tree ""

  let insert_in_folder tree name content =
    print_endline (pretty_print tree);
    match tree with
    | File _ -> tree
    | Folder (name', trees) -> Folder (name', File (name, content) :: trees)

  let get_folder_names tree =
    List.filter_map
      (fun t -> match t with File _ -> None | Folder (name, _) -> Some name)
      tree

  let get_folder_name tree =
    match tree with File _ -> None | Folder (name, _) -> Some name

  let is_folder tree = match tree with File _ -> false | Folder _ -> true

  let rec complete_tree tree path =
    match path with
    | [] -> tree
    | h :: rest ->
        if List.exists (fun t -> t = h) (get_folder_names tree) then
          List.filter_map
            (fun t ->
              if is_folder t then
                if get_folder_name t = Some h then complete_inner path t
                else None
              else Some t)
            tree
        else Folder (h, []) :: complete_tree tree rest

  and complete_inner path tree =
    match tree with
    | File _ -> None
    | Folder (name, trees) -> Some (Folder (name, complete_tree trees path))

  let rec insert_in_tree tree path name content =
    match path with
    | [] -> insert_in_folder tree name content
    | h :: rest -> (
        match tree with
        | File _ -> tree
        | Folder (name', trees) -> (
            match rest with
            | [] -> Folder (name', File (name, content) :: trees)
            | _ ->
                if name' = h then
                  Folder
                    ( name',
                      List.map
                        (fun t -> insert_in_tree t rest name content)
                        trees )
                else Folder (name, trees)))

  let tree_eg =
    Folder
      ( "root",
        [
          File ("a", "a");
          File ("b", "b");
          Folder ("x", []);
          Folder ("c", [ File ("d", "d"); File ("e", "e") ]);
        ] )

  let completed_tree = complete_tree [ tree_eg ] [ "root"; "c" ]
  let insert_test = insert_in_tree tree_eg [ "root"; "c" ] "f" "f"
  let t1 = insert_in_tree tree_eg [ "root"; "d" ] "f" "f"
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
