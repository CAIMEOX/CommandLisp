open Printf

let write_to_file (filename : string) (content : string) : unit =
  let oc = open_out_gen [ Open_append; Open_creat ] 0o666 filename in
  Printf.fprintf oc "%s\n" content;
  close_out oc

let append_to_file (filename : string) (content : string) : unit =
  let oc = open_out_gen [ Open_append; Open_creat ] 0o666 filename in
  Printf.fprintf oc "%s\n" content;
  close_out oc

let print_list l (ident : int) =
  List.iter
    (fun x ->
      print_string (String.make ident ' ');
      print_endline x)
    l

let calls = sprintf "calls %s"
let ( >> ) = sprintf "%s\n%s"
let flatten = String.concat "\n"

module McFunction = struct
  type mcfunction = { name : string; commands : string }

  let make_function : string -> string -> mcfunction = fun name commands -> { name; commands }

  type file_tree = { path : string; fn : mcfunction }

  let empty () = []
  let insert ft ftl = List.map (fun x -> if x.path = ft.path then ft else x) ftl

  (* won't create if do not exist *)
  let append ft ftl =
    List.map
      (fun x ->
        if x.path = ft.path then
          { x with fn = { x.fn with commands = x.fn.commands ^ "\n" ^ ft.fn.commands } }
        else x)
      ftl
end
