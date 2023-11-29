type prim =
  | Self
  | Add
  | Mul
  | Div
  | Sub (*| Eq | Lt | Gt | Le | Ge | And | Or | Not*)

type expr =
  | Cst of int
  | Prim of prim * expr list
  | Command of string
  | If of expr * expr * expr
  | Var of string
  | App of string * expr list
  | Letfn of string * string list * expr * expr
  | Let of string * expr * expr

open Core

let unwrap_atom = function Sexp.Atom s -> s | _ -> failwith "unwrap_atom"

let rec parse_sexp = function
  | Sexp.Atom s -> Cst (int_of_string s)
  | Sexp.List [ Sexp.Atom "+"; e1; e2 ] ->
      Prim (Add, [ parse_sexp e1; parse_sexp e2 ])
  | Sexp.List [ Sexp.Atom "*"; e1; e2 ] ->
      Prim (Mul, [ parse_sexp e1; parse_sexp e2 ])
  | Sexp.List [ Sexp.Atom "-"; e1; e2 ] ->
      Prim (Sub, [ parse_sexp e1; parse_sexp e2 ])
  | Sexp.List [ Sexp.Atom "/"; e1; e2 ] ->
      Prim (Div, [ parse_sexp e1; parse_sexp e2 ])
  | Sexp.List [ Sexp.Atom "if"; e1; e2; e3 ] ->
      If (parse_sexp e1, parse_sexp e2, parse_sexp e3)
  | Sexp.List
      [
        Sexp.Atom "let";
        Sexp.List (Sexp.Atom name :: args);
        Sexp.Atom "in";
        body;
        e2;
      ] ->
      Letfn (name, List.map args ~f:unwrap_atom, parse_sexp body, parse_sexp e2)
  | Sexp.List [ Sexp.Atom "run"; Sexp.Atom s ] -> Command s
  | _ -> failwith "parse error"

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
          ^ String.concat ~sep:" " (List.map exprs ~f:loop)
          ^ ")"
      | Var s -> s
      | App (f, args) ->
          "(" ^ f ^ " " ^ String.concat ~sep:" " (List.map args ~f:loop) ^ ")"
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
  | Prim (prim, exprs) -> Prim (prim, List.map exprs ~f:remove_funs)
  | Var s -> Var s
  | App (e1, e2) -> App (e1, List.map e2 ~f:remove_funs)
  | Letfn (_, _, _, scope) -> remove_funs scope
  | Let (s, e1, e2) -> Let (s, remove_funs e1, remove_funs e2)
  | If (e1, e2, e3) -> If (remove_funs e1, remove_funs e2, remove_funs e3)
  | Command _ -> failwith "unimplemented"

let rec collect_funs (expr : expr) : Flat.fn list =
  match expr with
  | Cst _ | Var _ -> []
  | Prim (_, exprs) -> List.concat_map exprs ~f:collect_funs
  | Let (_, e1, e2) -> collect_funs e1 @ collect_funs e2
  | Letfn (name, args, body, scope) ->
      ((name, args, remove_funs body) :: collect_funs body) @ collect_funs scope
  | If (e1, e2, e3) -> collect_funs e1 @ collect_funs e2 @ collect_funs e3
  | App (_, args) -> List.concat_map args ~f:collect_funs
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
    | Param s :: r -> if phys_equal s x then acc else loop r x (acc + 1)
    | Local s :: r -> if phys_equal s x then acc else loop r x (acc + 1)
    | Temp :: r -> loop r x (acc + 1)
  in
  loop venv x 0

let get_prim_op op name num =
  match op with
  | Add -> Arch.Add
  | Mul -> Arch.Mul
  | Sub -> Arch.Sub
  | Div -> Arch.Div
  | Self -> Arch.Call (name, num)

let rec compile_exprs venv exprs name num =
  let rec loop venv exprs acc =
    match exprs with
    | [] -> acc
    | e :: r -> loop (Temp :: venv) r (acc @ compile_expr venv e name num)
  in
  loop venv exprs []

and compile_expr (venv : venv) (expr : Flat.expr) (name : string) (num : int) =
  match expr with
  | Cst n -> [ Arch.Cst n ]
  | Prim (op, es) ->
      let es_code = compile_exprs venv es name num in
      let op_code = get_prim_op op name num in
      es_code @ [ op_code ]
  | Var x -> [ Arch.Var (vindex venv x) ]
  | App (f, args) ->
      compile_exprs venv args name num @ [ Arch.Call (f, List.length args) ]
  | Let (x, e1, e2) ->
      compile_expr venv e1 name num
      @ compile_expr (Local x :: venv) e2 name num
      @ [ Arch.Swap; Arch.Pop ]
  | If (cond, e1, e2) ->
      let else_label = LabelGen.else_fresh () in
      let end_label = LabelGen.end_fresh () in
      List.concat
        [
          compile_expr venv cond name num;
          [ Arch.IfZero else_label ];
          compile_expr venv e1 name num;
          [ Arch.Goto end_label; Arch.Label else_label ];
          compile_expr venv e2 name num;
          [ Arch.Label end_label ];
        ]

let compile_fun (fn : Flat.fn) =
  let name, args, body = fn in
  let num = List.length args in
  let venv = List.map args ~f:(fun x -> Param x) |> List.rev in
  List.concat
    [ Arch.Label name :: compile_expr venv body name num; [ Arch.Ret num ] ]

let preprocess (expr : expr) =
  ("main", [], remove_funs expr) :: collect_funs expr

let compile (funs : Flat.fn list) =
  [ Arch.Call ("main", 0); Arch.Exit ] @ List.concat_map funs ~f:compile_fun

let parse x = Sexp.of_string x |> parse_sexp
let preprocess_and_compile prog = prog |> preprocess |> compile
let res = Array.of_list

let string_of_prim = function
  | Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | Div -> "div"
  | Self -> "self"
