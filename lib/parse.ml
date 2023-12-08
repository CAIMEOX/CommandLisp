type prim = Self | Add | Mul | Div | Sub (*| Eq | Lt | Gt | Le | Ge | And | Or | Not*)

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
  | Sexp.List [ Sexp.Atom "+"; e1; e2 ] -> Prim (Add, [ parse_sexp e1; parse_sexp e2 ])
  | Sexp.List [ Sexp.Atom "*"; e1; e2 ] -> Prim (Mul, [ parse_sexp e1; parse_sexp e2 ])
  | Sexp.List [ Sexp.Atom "-"; e1; e2 ] -> Prim (Sub, [ parse_sexp e1; parse_sexp e2 ])
  | Sexp.List [ Sexp.Atom "/"; e1; e2 ] -> Prim (Div, [ parse_sexp e1; parse_sexp e2 ])
  | Sexp.List [ Sexp.Atom "if"; e1; e2; e3 ] -> If (parse_sexp e1, parse_sexp e2, parse_sexp e3)
  | Sexp.List [ Sexp.Atom "let"; Sexp.List (Sexp.Atom name :: args); Sexp.Atom "in"; body; e2 ] ->
      Letfn (name, List.map args ~f:unwrap_atom, parse_sexp body, parse_sexp e2)
  | Sexp.List [ Sexp.Atom "run"; Sexp.Atom s ] -> Command s
  | _ -> failwith "parse error"

let parse x = Sexp.of_string x |> parse_sexp
