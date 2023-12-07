open Printf

module Position = struct
  type position =
    | Absolute of int * int * int
    | Relative of int * int * int
    | View of int * int * int

  type direction = X | Y | Z | NX | NY | NZ

  let counter_direction d =
    match d with X -> NX | Y -> NY | Z -> NZ | NX -> X | NY -> Y | NZ -> Z

  let rint x = if x = 0 then "~" else sprintf "~%d" x

  let string_of_position (pos : position) =
    match pos with
    | Absolute (x, y, z) -> sprintf "%d %d %d" x y z
    | Relative (x, y, z) -> sprintf "%s %s %s" (rint x) (rint y) (rint z)
    | View (x, y, z) -> sprintf "^%d ^%d ^%d" x y z

  let direction_to_position d offset =
    match d with
    | X -> Relative (offset, 0, 0)
    | Y -> Relative (0, offset, 0)
    | Z -> Relative (0, 0, offset)
    | NX -> Relative (-offset, 0, 0)
    | NY -> Relative (0, -offset, 0)
    | NZ -> Relative (0, 0, -offset)
end

module TargetSelector = struct
  type variable = Nearest | Random | AllPlayers | AllEntities | Self

  type argument =
    | X of int
    | Y of int
    | Z of int
    | Dx of int
    | Dy of int
    | Dz of int
    | R of float
    | Rm of float
    | Rx of int
    | Rxm of int
    | Tag of string
    | Type of string
    | Family of string
    | Mode of string
    | Count of int
    | L of int
    | Lm of int
    | Score of (string * int) list

  type selector =
    | VarArg of variable * argument list
    | Var of variable
    | Name of string

  let string_of_variable = function
    | Nearest -> "@p"
    | Random -> "@r"
    | AllPlayers -> "@a"
    | AllEntities -> "@e"
    | Self -> "@s"

  let string_of_argument a =
    match a with
    | X x -> sprintf "x=%d" x
    | Y y -> sprintf "y=%d" y
    | Z z -> sprintf "z=%d" z
    | Dx dx -> sprintf "dx=%d" dx
    | Dy dy -> sprintf "dy=%d" dy
    | Dz dz -> sprintf "dz=%d" dz
    | R r -> sprintf "r=%.1f" r
    | Rm rm -> sprintf "rm=%.1f" rm
    | Rx rx -> sprintf "rx=%d" rx
    | Rxm rxm -> sprintf "rxm=%d" rxm
    | Tag tag -> sprintf "tag=%s" tag
    | Type ty -> sprintf "type=%s" ty
    | Family family -> sprintf "family=%s" family
    | Mode mode -> sprintf "mode=%s" mode
    | Count count -> sprintf "c=%d" count
    | L l -> sprintf "l=%d" l
    | Lm lm -> sprintf "lm=%d" lm
    | Score scores ->
        "scores={"
        ^ (List.map (fun (s, v) -> sprintf "%s=%d" s v) scores
          |> String.concat ",")
        ^ "}"

  let string_of_selector selector =
    match selector with
    | VarArg (var, args) ->
        sprintf "%s[%s]" (string_of_variable var)
          (List.map string_of_argument args |> String.concat ",")
    | Var var -> string_of_variable var
    | Name name -> name
end

module ScoreBoard = struct
  open TargetSelector

  type operation = Add | Set | Sub | Mul | Div | Mod | Min | Max | Swap
  type target = selector

  let string_of_target = string_of_selector

  type objective =
    | List
    | Add of string * string
    (* objective, name *)
    | Remove of string (* to do: setdisplay *)

  type player =
    | List of string
    | Set of target * string * int
    | Add of target * string * int
    | Remove of target * string * int
    | Random of target * string * int * int
    | Reset of target * string
    | Test of target * string * int * int
    | Operation of target * string * operation * target * string

  type scoreboard = Objective of objective | Player of player

  let string_of_operation (op : operation) =
    match op with
    | Add -> "+="
    | Set -> "="
    | Sub -> "-="
    | Mul -> "*="
    | Div -> "/="
    | Mod -> "%="
    | Min -> "<"
    | Max -> ">"
    | Swap -> "><"

  let string_of_objective (obj : objective) =
    match obj with
    | List -> "list"
    | Add (obj, name) -> sprintf "add %s dummy %s" obj name
    | Remove obj -> sprintf "remove %s" obj

  let string_of_player player =
    match player with
    | List obj -> sprintf "list %s" obj
    | Set (target, obj, value) ->
        sprintf "set %s %s %d" (string_of_target target) obj value
    | Add (target, obj, value) ->
        sprintf "add %s %s %d" (string_of_target target) obj value
    | Remove (target, obj, value) ->
        sprintf "remove %s %s %d" (string_of_target target) obj value
    | Random (target, obj, min, max) ->
        sprintf "random %s %s %d %d" (string_of_target target) obj min max
    | Reset (target, obj) -> sprintf "reset %s %s" (string_of_target target) obj
    | Test (target, obj, min, max) ->
        sprintf "test %s %s %d %d" (string_of_target target) obj min max
    | Operation (target, obj, op, target2, obj2) ->
        sprintf "operation %s %s %s %s %s" (string_of_target target) obj
          (string_of_operation op) (string_of_target target2) obj2

  let string_of_scoreboard scb =
    match scb with
    | Objective obj ->
        sprintf "scoreboard objectives %s" (string_of_objective obj)
    | Player player -> sprintf "scoreboard players %s" (string_of_player player)
end

module Tag = struct
  open TargetSelector

  type tag =
    | Add of selector * string
    | Remove of selector * string
    | List of selector

  let string_of_tag t =
    match t with
    | Add (sel, tag) -> sprintf "tag %s add %s" (string_of_selector sel) tag
    | Remove (sel, tag) ->
        sprintf "tag %s remove %s" (string_of_selector sel) tag
    | List sel -> sprintf "tag %s list" (string_of_selector sel)
end

module Summon = struct
  type summon = Summon of string * Position.position

  let string_of_summon sn =
    match sn with
    | Summon (name, pos) ->
        sprintf "summon %s %s" name (Position.string_of_position pos)
end

module Function = struct
  let string_of_function name = sprintf "function %s" name
end

module Execute = struct
  open TargetSelector
  open Position

  type range = int * int
  type compare_operation = Eq | Lt | Le | Gt | Ge

  let string_of_compare_operation = function
    | Eq -> "="
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="

  type modify =
    | Align of string
    | Anchored of string
    | As of selector
    | At of selector
    | FacingPos of position
    | FacingEntity of selector * string
    | Positioned of position
    | PositionedAs of selector
    | Dimension of string

  type option = If of cond | Unless of cond
  and cond = Score of score

  and score =
    | Operation of selector * string * compare_operation * selector * string
    | MatchesValue of selector * string * int
    | MatchesRange of selector * string * range

  let string_of_score s =
    match s with
    | Operation (sel1, obj1, op, sel2, obj2) ->
        sprintf "%s %s %s %s %s" (string_of_selector sel1) obj1
          (string_of_compare_operation op)
          (string_of_selector sel2) obj2
    | MatchesValue (sel, obj, value) ->
        sprintf "%s %s matches %d" (string_of_selector sel) obj value
    | MatchesRange (sel, obj, (min, max)) ->
        sprintf "%s %s matches %d..%d" (string_of_selector sel) obj min max

  let string_of_cond c =
    match c with Score s -> sprintf "score %s" (string_of_score s)

  let string_of_option o =
    match o with
    | If c -> sprintf "if %s" (string_of_cond c)
    | Unless c -> sprintf "unless %s" (string_of_cond c)

  let string_of_modify m =
    match m with
    | Align a -> sprintf "align %s" a
    | Anchored a -> sprintf "anchored %s" a
    | As sel -> sprintf "as %s" (string_of_selector sel)
    | At sel -> sprintf "at %s" (string_of_selector sel)
    | FacingPos pos -> sprintf "facing %s" (string_of_position pos)
    | FacingEntity (sel, anchor) ->
        sprintf "facing entity %s %s" (string_of_selector sel) anchor
    | Positioned pos -> sprintf "positioned %s" (string_of_position pos)
    | PositionedAs sel -> sprintf "positioned as %s" (string_of_selector sel)
    | Dimension dim -> sprintf "dimension %s" dim

  type subcommand = Modify of modify | Run of string | Option of option
  type execute = Execute of subcommand list

  let string_of_subcommand sc =
    match sc with
    | Modify m -> string_of_modify m
    | Run s -> sprintf "run %s" s
    | Option o -> string_of_option o

  let string_of_execute (Execute scs) =
    sprintf "execute %s" (List.map string_of_subcommand scs |> String.concat " ")
end

let get_offset_dire (x : Position.direction) i =
  let open TargetSelector in
  match x with
  | X -> Dx i
  | Y -> Dy i
  | Z -> Dz i
  | NX -> Dx (-i)
  | NY -> Dy (-i)
  | NZ -> Dz (-i)
