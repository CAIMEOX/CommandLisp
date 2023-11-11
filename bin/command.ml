module ScoreBoard = struct
  open Printf
  let objective_add = sprintf "scoreboard objectives add %s"
  let player_unary_op = sprintf "scoreboard players %s %s %d"
  let unary_add = player_unary_op "add"
  let unary_set = player_unary_op "set"
  let unary_sub = player_unary_op "remove"
  let player_bin_op op dest src = sprintf "scoreboard players operation %s %s %s" dest op src
  let binop_assign = player_bin_op "="
  let binop_add = player_bin_op "+="
  let binop_sub = player_bin_op "-="
  let binop_mul = player_bin_op "*="
  let binop_div = player_bin_op "/="
  let binop_mod = player_bin_op "%="
  let binop_min = player_bin_op "<"
  let binop_max = player_bin_op ">"
  let binop_swap = player_bin_op "><"
end

module Execute = struct 
  open Printf
  (* make a monad that compose two sub instructions*)
  let (>>) = sprintf "%s %s"
  let cond_score_binop op dest src = sprintf "if score %s %s %s" dest op src
  let cond_score_range dest = sprintf "if score %s matches %d..%d" dest
  let cond_score_is = sprintf "if score %s matches %d"
  let cond_score_eq = cond_score_binop "="
  let cond_score_lt = cond_score_binop "<"
  let cond_score_gt = cond_score_binop ">"
  let cond_score_le = cond_score_binop "<="
  let cond_score_ge = cond_score_binop ">="
  let execute = sprintf "execute %s run %s"
end

module Util = struct
  open Printf
  let write_to_file (filename: string) (content: string): unit =
    let oc = open_out filename in
    Printf.fprintf oc "%s\n" content;
    close_out oc

  let append_to_file (filename: string) (content: string): unit =
    let oc = open_out_gen [Open_append; Open_creat] 0o666 filename in
    Printf.fprintf oc "%s\n" content;
    close_out oc

  let calls = sprintf "calls %s"
  let (>>) = sprintf "%s\n%s"
  let flatten = String.concat "\n"
end

module Register = struct
  open ScoreBoard
  open Printf
  open Util
  let register = sprintf "cpu %s"
  let xi = Printf.sprintf "x%d"
  let ti = Printf.sprintf "t%d"
  let ai = Printf.sprintf "a%d"
  let reg_op op r1 r2 = ScoreBoard.player_bin_op op (register r1) (register r2)
  let reg_init regs = List.init 5 (fun i -> objective_add (regs i) >> unary_set (register (regs i)) 0)
  let reg_set reg value = unary_set (register reg) value
  let reg_add reg value = unary_add (register reg) value
  let pointers: string list = (objective_add "rp" >> unary_set (register "rp") 0) :: [objective_add "sp" >> unary_set (register "sp") 0]
  let init = reg_init xi @ reg_init ti @ reg_init ai @ pointers
end

module StackFrame = struct
  open ScoreBoard
  open Printf
  open Execute
  open Util
  let stack = sprintf "stack %s"
  let run_function = sprintf "function %s"
  let name = sprintf "stack_%s_%s"
  let file_name x y = name x y |> sprintf "functions/%s.mcfunction" 
  let sindex = Printf.sprintf "stack_%d"
  let init size = List.init size (fun i -> objective_add (sindex i) >> unary_set (stack (sindex i)) 0)
  let search_value register size op =
    List.init size (fun i -> execute (cond_score_is register i) (op (stack (sindex i))))
  
  let generate_push size reg = search_value "cpu sp" size (fun x -> binop_assign x (Register.register reg))
  let generate_pop size reg = search_value "cpu sp" size (binop_assign (Register.register reg))
  
  let generate_access size reg_target reg_save = 
    let helper size op = List.init size (fun i -> execute (cond_score_eq reg_target (stack (sindex i))) (op (stack (sindex i)))) in
    helper size (binop_assign (Register.register reg_save))
  let push size reg = 
    write_to_file (file_name "push" reg) (generate_push size reg |> flatten >> unary_add (Register.register "sp") 1);
    run_function (name "push" reg)
  let pop size reg = 
    write_to_file (file_name "pop" reg) (unary_sub (Register.register "sp") 1 >> (generate_pop size reg |> flatten));
    run_function (name "pop" reg)
  let access size reg index_in_reg = 
    write_to_file (file_name "access" reg) (generate_access size (Register.register index_in_reg) (reg) |> flatten);
    run_function (name "access" reg)
end

(* Calling a function:
  | Ret.push return address (int)
  | address connect to a function (label)
  | push arguments (int) 
  | jump to function (label)
  | Ret.pop return address (int) to register x0
  | run the connected function (label)
*)
module RetStack = struct
  open ScoreBoard
  open Printf
  open Execute
  open Util
  let stack = sprintf "ret_stack %s"
  let run_function = sprintf "function %s"
  let name = sprintf "ret_stack_%s_%s"
  let file_name x y = name x y |> sprintf "functions/%s.mcfunction" 
  let sindex = Printf.sprintf "ret_stack_%d"
  let init size = List.init size (fun i -> objective_add (sindex i) >> unary_set (stack (sindex i)) 0)
  let search_value register size op =
    List.init size (fun i -> execute (cond_score_is register i) (op (stack (sindex i))))
  
  let generate_push size reg = search_value "cpu rp" size (fun x -> binop_assign x (Register.register reg))
  let generate_pop size reg = search_value "cpu rp" size (binop_assign (Register.register reg))
  
  let generate_access size reg_target reg_save = 
    let helper size op = List.init size (fun i -> execute (cond_score_eq reg_target (stack (sindex i))) (op (stack (sindex i)))) in
    helper size (binop_assign (Register.register reg_save))
  let push size reg = 
    write_to_file (file_name "push" reg) (generate_push size reg |> flatten >> unary_add (Register.register "rp") 1);
    run_function (name "push" reg)
  let pop size reg = 
    write_to_file (file_name "pop" reg) (unary_sub (Register.register "rp") 1 >> (generate_pop size reg |> flatten));
    run_function (name "pop" reg)
  let run_linked_function reg fn_index fn_label =
    execute (cond_score_is (Register.register reg) fn_index) (run_function fn_label)
  let link reg fn_index fn_label =
    append_to_file (file_name "control" reg) (run_linked_function reg fn_index fn_label);
    run_function (name "call" reg)
end