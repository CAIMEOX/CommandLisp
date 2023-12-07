open Command_lisp
open Vm
open Parse
open Compile

let weclome_message = "Welcome to Command Lisp. \nType 'exit' to quit."

let compile_and_run content =
  content |> parse |> preprocess_and_compile |> Array.of_list |> initVm |> run

let repl () =
  print_endline weclome_message;
  let rec loop () =
    print_string "> ";
    try
      let line = read_line () in
      if line = "exit" then print_endline "Bye!"
      else
        try
          let r = line |> compile_and_run |> string_of_int in
          print_endline r;
          loop ()
        with e ->
          print_endline (Printexc.to_string e);
          loop ()
    with End_of_file -> print_endline "Bye!"
  in
  loop ()

let load_file file =
  let ic = open_in file in
  let content = ref "" in
  let rec loop () =
    try
      let line = input_line ic in
      content := !content ^ line ^ "\n";
      loop ()
    with End_of_file -> ()
  in
  loop ();
  close_in ic;
  try compile_and_run !content |> ignore
  with e -> print_endline (Printexc.to_string e)

let usage = "Usage: clc [repl|run <file>]"

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: "repl" :: _ -> repl ()
  | _ :: "run" :: file :: _ -> load_file file
  | _ -> print_endline usage
