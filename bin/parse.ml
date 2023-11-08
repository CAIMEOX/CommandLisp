type expr = Cst of int | Add of expr * expr | Mul of expr * expr

open Vm

let rec compile (expr : expr) : instr list =
  match expr with
  | Cst n -> [ Cst n ]
  | Add (e1, e2) ->
      List.append (List.append (compile e1) (compile e2)) [ Add ("a0", "a1") ]
  | Mul (e1, e2) ->
      List.append (List.append (compile e1) (compile e2)) [ Mul ("a0", "a1") ]
