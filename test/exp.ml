open Command_lisp
open Parse

let fact i =
  Letfn
    ( "fact",
      [ "n" ],
      If
        ( Var "n",
          Prim
            ( Mul,
              [ Var "n"; Prim (Self, [ Prim (Add, [ Var "n"; Cst (-1) ]) ]) ] ),
          Cst 1 ),
      App ("fact", [ Cst i ]) )

let fib i =
  Letfn
    ( "fib",
      [ "n" ],
      If
        ( Var "n",
          If
            ( Prim (Add, [ Var "n"; Cst (-1) ]),
              Prim
                ( Add,
                  [
                    Prim (Self, [ Prim (Add, [ Var "n"; Cst (-1) ]) ]);
                    Prim (Self, [ Prim (Add, [ Var "n"; Cst (-2) ]) ]);
                  ] ),
              Cst 1 ),
          Cst 1 ),
      App ("fib", [ Cst i ]) )

open Command
open ScoreBoard

let scb_add = Objective (Add ("caimeo", "caimeo")) |> string_of_scoreboard
let scb_remove = Objective (Remove "caimeo") |> string_of_scoreboard

let scb_player_add =
  Player (Add (Name "caimeo", "obj", 1)) |> string_of_scoreboard

let scb_player_op_add =
  Player (Operation (Name "caimeo", "obj", Add, Name "lampese", "obj"))
  |> string_of_scoreboard

let scb_player_op_set =
  Player (Operation (Name "caimeo", "obj", Set, Name "lampese", "obj"))
  |> string_of_scoreboard

open TargetSelector

let ts1 = VarArg (AllEntities, [ Tag "wow" ]) |> string_of_selector

let ts2 =
  VarArg
    (AllEntities, [ Tag "wow"; Count 1; R 0.5; Score [ ("a", 1); ("b", 2) ] ])
  |> string_of_selector

open Execute

let exe1 =
  Execute [ Modify (As (Name "CAIMEO")); Run "say Hello World" ]
  |> string_of_execute

let exe1_str = "execute as CAIMEO run say Hello World"

let exe2 =
  Execute
    [
      Option (If (Score (MatchesRange (Name "caimeo", "obj", (1, 2)))));
      Run "tag @s A";
    ]
  |> string_of_execute

let exe2_str = "execute if score caimeo obj matches 1..2 run tag @s A"

let exe3 =
  Execute
    [
      Option
        (Unless
           (Score
              (Operation (Name "caimeo", "obj1", Eq, Name "lampese", "obj2"))));
      Modify (Positioned Position.(Relative (1, 2, 3)));
      Run "say A";
    ]
  |> string_of_execute

let exe3_str =
  "execute unless score caimeo obj1 = lampese obj2 positioned ~1 ~2 ~3 run say \
   A"
