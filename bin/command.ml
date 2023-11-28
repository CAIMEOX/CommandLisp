module ScoreBoard = struct
  open Printf

  let objective_add = sprintf "scoreboard objectives add %s"
  let player_unary_op = sprintf "scoreboard players %s %s %d"
  let unary_add = player_unary_op "add"
  let unary_set = player_unary_op "set"
  let unary_sub = player_unary_op "remove"

  let player_bin_op op dest src =
    sprintf "scoreboard players operation %s %s %s" dest op src

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
  let ( >> ) = sprintf "%s %s"
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
