open Parse

(* let fact i =
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

   let p_fact = preprocess_and_compile (fact 10)
   let res = Array.of_list p_fact |> Vm.initVm |> Vm.run
   let () = Printf.printf "fact(10) = %d\n" res *)

let fib i =
  Letfn
    ( "fact",
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
      App ("fact", [ Cst i ]) )

let p_fib = preprocess_and_compile (fib 25)
let res1 = Array.of_list p_fib |> Vm.initVm |> Vm.run
let () = Printf.printf "fib(25) = %d\n" res1

let fact_tail x =
  Letfn
    ( "fact_tail",
      [ "n"; "acc" ],
      If
        ( Var "n",
          Prim
            ( Self,
              [
                Prim (Add, [ Var "n"; Cst (-1) ]);
                Prim (Add, [ Var "acc"; Var "n" ]);
              ] ),
          Var "acc" ),
      Letfn
        ( "fact",
          [ "n" ],
          App ("fact_tail", [ Var "n"; Cst 1 ]),
          App ("fact", [ Cst x ]) ) )

let p_fact_tail = preprocess_and_compile (fact_tail 25)
let res2 = Array.of_list p_fact_tail |> Vm.initVm |> Vm.run
let () = Printf.printf "fact(25) = %d\n" res2
