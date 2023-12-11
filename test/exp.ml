open Command_lisp
open Parse

let fact i =
  Letfn
    ( "fact",
      [ "n" ],
      If
        (Var "n", Prim (Mul, [ Var "n"; Prim (Self, [ Prim (Add, [ Var "n"; Cst (-1) ]) ]) ]), Cst 1),
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
