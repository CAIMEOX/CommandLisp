open Parse

module Casm = struct
  type instr =
    | Cst of int
    | Pop
    | Swap
    | Add
    | Mul
    | Sub
    | Div
    | Call of string * int
    | Var of int
    | Ret of int
    | Label of string
    | IfZero of string
    | Goto of string
    | Syscall
    | Exit

  type string_list_map = (string, instr list) Hashtbl.t

  let counter = ref 0
  let incr_counter () = counter := !counter + 1

  let is_num x =
    try
      ignore (int_of_string x);
      true
    with _ -> false

  let refresh_counter () = counter := 0

  let call_counter prefix () =
    let n = !counter in
    "fp_next_call_" ^ prefix ^ "_" ^ string_of_int n

  let label_after_call instrs =
    let () = refresh_counter () in
    let rec loop rest res =
      match rest with
      | [] -> res
      | IfZero t :: rest -> loop rest (res @ [ IfZero t; Label ("counter" ^ t) ])
      | Goto t :: rest -> loop rest (res @ [ Goto t ])
      | Call (l, _) :: rest ->
          let () = incr_counter () in
          loop rest
            (res @ [ Call (l, !counter); Label (string_of_int !counter) ])
      | x :: rest -> loop rest (res @ [ x ])
    in
    loop instrs []

  let collect_labels instrs =
    let rec loop rest instrs res =
      match rest with
      | [] -> res
      | Label l :: rest ->
          if is_num l then loop rest [] ([ (l, instrs) ] @ res)
          else loop rest [ Label l ] ([ (l, instrs) ] @ res)
      | x :: rest -> loop rest (x :: instrs) res
    in
    loop instrs [] []

  let dead_code_elimination (instrs : instr list) (funs : string list) =
    let rec aux ins res =
      match ins with
      | [] -> res
      (* | IfZero l :: Label _ :: _ -> res @ [ IfZero l ] *)
      | Exit :: _ -> res @ [ Exit ]
      | Goto l :: _ -> res @ [ Goto l ]
      | Ret n :: _ -> res @ [ Ret n ]
      | Label l :: [] ->
          if List.mem l funs then res @ [ Goto l ] else res @ [ Label l ]
      | x :: rest -> aux rest (res @ [ x ])
    in
    aux instrs []

  let collect_n2s ft =
    let rec aux ft res =
      match ft with
      | [] -> res
      | (n, l, _) :: rest -> aux rest (res @ [ (n, l) ])
    in
    aux ft []

  let rec find_tuple_snd tpl name =
    match tpl with
    | (n, l) :: rest ->
        if name = l then string_of_int n else find_tuple_snd rest name
    | [] -> failwith "unbound label"

  let numberize_instrs ins kvs =
    let rec aux ins res =
      match ins with
      | Call (l, n) :: rest ->
          aux rest (res @ [ Call (find_tuple_snd kvs l, n) ])
      | IfZero l :: rest -> aux rest (res @ [ IfZero (find_tuple_snd kvs l) ])
      | Goto l :: rest -> aux rest (res @ [ Goto (find_tuple_snd kvs l) ])
      | Label l :: rest -> aux rest (res @ [ Label (find_tuple_snd kvs l) ])
      | x :: rest -> aux rest (res @ [ x ])
      | [] -> res
    in
    aux ins []

  let numberize_labels ft =
    let rec aux ft res =
      match ft with
      | [] -> res
      | (l, ins) :: rest ->
          if is_num l then aux rest (res @ [ (int_of_string l, l, ins) ])
          else
            let () = incr_counter () in
            aux rest (res @ [ (!counter, l, ins) ])
    in
    let lst = aux ft [] in
    let n2s = collect_n2s lst in
    List.map (fun (n, _l, ins) -> (string_of_int n, numberize_instrs ins n2s)) lst

  let compile_to_function instrs =
    let funs = instrs |> label_after_call |> List.rev |> collect_labels in
    let funs =
      List.map
        (fun (l, instrs) ->
          (l, dead_code_elimination instrs (List.map fst funs)))
        funs
    in
    let funs = numberize_labels funs in
    let map = Hashtbl.create (List.length funs) in
    List.iter (fun (l, instrs) -> Hashtbl.add map l instrs) funs;
    map
end

module Flat = struct
  type expr =
    | Cst of int
    | Prim of prim * expr list
    | Var of string
    | App of string * expr list
    | Let of string * expr * expr
    | If of expr * expr * expr

  type fn = string * string list * expr

  let rec string_of_expr expr =
    let rec loop expr =
      match expr with
      | Cst n -> string_of_int n
      | Prim (prim, exprs) ->
          "(" ^ string_of_prim prim ^ " "
          ^ String.concat " " (List.map loop exprs)
          ^ ")"
      | Var s -> s
      | App (f, args) ->
          "(" ^ f ^ " " ^ String.concat " " (List.map loop args) ^ ")"
      | Let (x, e1, e2) -> "(let (" ^ x ^ " " ^ loop e1 ^ ") " ^ loop e2 ^ ")"
      | If (e1, e2, e3) ->
          "(if " ^ loop e1 ^ " " ^ loop e2 ^ " " ^ loop e3 ^ ")"
    in
    loop expr

  and string_of_prim = function
    | Add -> "+"
    | Mul -> "*"
    | Sub -> "-"
    | Div -> "/"
    | Self -> "self"
end

type var = Param of string | Local of string | Temp
type venv = var list

let rec remove_funs (expr : expr) : Flat.expr =
  match expr with
  | Cst n -> Cst n
  | Prim (prim, exprs) -> Prim (prim, List.map remove_funs exprs)
  | Var s -> Var s
  | App (e1, e2) -> App (e1, List.map remove_funs e2)
  | Letfn (_, _, _, scope) -> remove_funs scope
  | Let (s, e1, e2) -> Let (s, remove_funs e1, remove_funs e2)
  | If (e1, e2, e3) -> If (remove_funs e1, remove_funs e2, remove_funs e3)
  | Command _ -> failwith "unimplemented"

let rec collect_funs (expr : expr) : Flat.fn list =
  match expr with
  | Cst _ | Var _ -> []
  | Prim (_, exprs) -> List.concat_map collect_funs exprs
  | Let (_, e1, e2) -> collect_funs e1 @ collect_funs e2
  | Letfn (name, args, body, scope) ->
      ((name, args, remove_funs body) :: collect_funs body) @ collect_funs scope
  | If (e1, e2, e3) -> collect_funs e1 @ collect_funs e2 @ collect_funs e3
  | App (_, args) -> List.concat_map collect_funs args
  | Command _ -> failwith "unimplemented"

module LabelGen = struct
  let counter = ref 0

  let next prefix () =
    let n = !counter in
    counter := n + 1;
    prefix ^ string_of_int n

  let else_fresh = next "_else_"
  let end_fresh = next "_exit_"
end

let vindex (venv : venv) (x : string) =
  let rec loop v x acc =
    match v with
    | [] -> failwith "unbound variable"
    | Param s :: r -> if s = x then acc else loop r x (acc + 1)
    | Local s :: r -> if s = x then acc else loop r x (acc + 1)
    | Temp :: r -> loop r x (acc + 1)
  in
  loop venv x 0

let get_prim_op op name =
  match op with
  | Add -> Casm.Add
  | Mul -> Casm.Mul
  | Sub -> Casm.Sub
  | Div -> Casm.Div
  | Self -> Casm.Call (name, 0)

let rec compile_exprs venv exprs name num =
  let rec loop venv exprs acc =
    match exprs with
    | [] -> acc
    | e :: r -> loop (Temp :: venv) r (acc @ compile_expr venv e name num)
  in
  loop venv exprs []

and compile_expr (venv : venv) (expr : Flat.expr) (name : string) (num : int) =
  match expr with
  | Cst n -> [ Casm.Cst n ]
  | Prim (op, es) ->
      let es_code = compile_exprs venv es name num in
      let op_code = get_prim_op op name in
      es_code @ [ op_code ]
  | Var x -> [ Casm.Var (vindex venv x) ]
  | App (f, args) ->
      compile_exprs venv args name num @ [ Casm.Call (f, List.length args) ]
  | Let (x, e1, e2) ->
      compile_expr venv e1 name num
      @ compile_expr (Local x :: venv) e2 name num
      @ [ Casm.Swap; Casm.Pop ]
  | If (cond, e1, e2) ->
      let else_label = LabelGen.else_fresh () in
      let end_label = LabelGen.end_fresh () in
      List.concat
        [
          compile_expr venv cond name num;
          [ Casm.IfZero else_label ];
          compile_expr venv e1 name num;
          [ Casm.Goto end_label; Casm.Label else_label ];
          compile_expr venv e2 name num;
          [ Casm.Label end_label ];
        ]

let compile_fun (fn : Flat.fn) =
  let name, args, body = fn in
  let num = List.length args in
  let venv = List.map (fun x -> Param x) args |> List.rev in
  List.concat
    [ Casm.Label name :: compile_expr venv body name num; [ Casm.Ret num ] ]

let preprocess (expr : expr) =
  ("main", [], remove_funs expr) :: collect_funs expr

let compile (funs : Flat.fn list) =
  [ Casm.Label "0"; Casm.Call ("main", 0); Casm.Exit ]
  @ List.concat_map compile_fun funs

let preprocess_and_compile prog = prog |> preprocess |> compile

let compile_to_function prog =
  let open Casm in
  prog |> preprocess_and_compile |> label_after_call |> List.rev
  |> collect_labels

let string_of_prim = function
  | Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | Div -> "div"
  | Self -> "self"
