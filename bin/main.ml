let repl () =
  let rec loop () =
    print_string "> ";
    let line = read_line () in
    print_endline line;
    loop ()
  in
  loop ()

let load_file file =
  let ic = open_in file in
  let rec loop () =
    try
      let line = input_line ic in
      print_endline line;
      loop ()
    with End_of_file -> ()
  in
  loop ();
  close_in ic

let usage = "Usage: command_lisp [repl|run <file>]"

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: "repl" :: _ -> repl ()
  | _ :: "run" :: file :: _ -> load_file file
  | _ -> print_endline usage
