let number_digit_len x =
  let rec loop x acc = if x < 10 then acc + 1 else loop (x / 10) (acc + 1) in
  loop x 0

let fmt_align_number n max =
  let s = string_of_int n in
  let len = String.length s in
  let padding = String.make (number_digit_len max - len) ' ' in
  padding ^ s

open Compile
open Casm

let string_of_instr instr =
  match instr with
  | Cst i -> "Cst " ^ string_of_int i
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Var i -> "Var " ^ string_of_int i
  | Pop -> "Pop"
  | Swap -> "Swap"
  | Label l -> "Label " ^ l
  | Ret n -> "Ret " ^ string_of_int n
  | Call (f, n) -> "Call " ^ f ^ " " ^ string_of_int n
  | IfZero l -> "IfZero " ^ l
  | Goto l -> "Goto " ^ l
  | Exit -> "Exit"
  | Syscall -> "Syscall"

let rec format_instr instrs : string =
  match instrs with
  | [] -> ""
  | instr :: instrs -> (
      match instr with
      | Pop -> "Pop\n" ^ format_instr instrs
      | Swap -> "Swap\n" ^ format_instr instrs
      | Add -> "Add\n" ^ format_instr instrs
      | Mul -> "Mul\n" ^ format_instr instrs
      | Sub -> "Sub\n" ^ format_instr instrs
      | Div -> "Div\n" ^ format_instr instrs
      | Call (label, n) -> "Call " ^ label ^ " " ^ string_of_int n ^ "\n" ^ format_instr instrs
      | Ret n -> "Ret " ^ string_of_int n ^ "\n" ^ format_instr instrs
      | IfZero r -> "IfZero " ^ r ^ "\n" ^ format_instr instrs
      | Var n -> "Var " ^ string_of_int n ^ "\n" ^ format_instr instrs
      | Cst i -> "Cst " ^ string_of_int i ^ "\n" ^ format_instr instrs
      | Label l -> "Label " ^ l ^ ":\n" ^ format_instr instrs
      | Syscall -> "Syscall\n" ^ format_instr instrs
      | Goto i -> "Goto " ^ i ^ "\n" ^ format_instr instrs
      | Exit -> "Exit\n" ^ format_instr instrs)

let pretty_instrs instrs =
  let m = List.length instrs in
  List.mapi
    (fun i instr -> Printf.sprintf "%s | %s" (fmt_align_number i m) (string_of_instr instr))
    instrs
  |> String.concat "\n"

let format_labeled tbl =
  List.map
    (fun (l, instrs) ->
      Printf.sprintf "[%s]: \n%s" l
        (List.map (fun t -> "\t" ^ string_of_instr t) instrs |> String.concat "\n"))
    tbl

let print_labeled tbl = List.iter print_endline (format_labeled tbl)
let pretty_print instrs = print_endline (pretty_instrs instrs)
