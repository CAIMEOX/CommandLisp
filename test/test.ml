open Command_lisp
open Compile
open Exp
open Vm

module Fact = struct
  let test_fact i =
    let instrs = preprocess_and_compile (fact i) in
    Array.of_list instrs |> Vm.initVm |> Vm.run

  let test_fact_real i =
    let instrs = preprocess_and_compile (fact i) in
    RealVm.compile_and_run instrs
end

module Fib = struct
  let test_fib i =
    let instrs = preprocess_and_compile (fib i) in
    Array.of_list instrs |> Vm.initVm |> Vm.run

  let test_fib_real i =
    let instrs = preprocess_and_compile (fib i) in
    RealVm.compile_and_run instrs
end

let () =
  let open Alcotest in
  let open Fact in
  run "VM"
    [
      ( "fact_stack_vm",
        [
          test_case "fact 0" `Quick (fun () ->
              check int "fact 0" 1 (test_fact 0));
          test_case "fact 1" `Quick (fun () ->
              check int "fact 1" 1 (test_fact 1));
          test_case "fact 2" `Quick (fun () ->
              check int "fact 2" 2 (test_fact 2));
          test_case "fact 3" `Quick (fun () ->
              check int "fact 3" 6 (test_fact 3));
          test_case "fact 4" `Quick (fun () ->
              check int "fact 4" 24 (test_fact 4));
          test_case "fact 5" `Quick (fun () ->
              check int "fact 5" 120 (test_fact 5));
          test_case "fact 6" `Quick (fun () ->
              check int "fact 6" 720 (test_fact 6));
          test_case "fact 7" `Quick (fun () ->
              check int "fact 7" 5040 (test_fact 7));
          test_case "fact 8" `Quick (fun () ->
              check int "fact 8" 40320 (test_fact 8));
          test_case "fact 9" `Quick (fun () ->
              check int "fact 9" 362880 (test_fact 9));
          test_case "fact 10" `Quick (fun () ->
              check int "fact 10" 3628800 (test_fact 10));
        ] );
      ( "fact_fun_pointer_vm",
        [
          test_case "fact 0" `Quick (fun () ->
              check int "fact 0" 1 (Fact.test_fact_real 0));
          test_case "fact 1" `Quick (fun () ->
              check int "fact 1" 1 (Fact.test_fact_real 1));
          test_case "fact 2" `Quick (fun () ->
              check int "fact 2" 2 (Fact.test_fact_real 2));
          test_case "fact 3" `Quick (fun () ->
              check int "fact 3" 6 (Fact.test_fact_real 3));
          test_case "fact 4" `Quick (fun () ->
              check int "fact 4" 24 (Fact.test_fact_real 4));
          test_case "fact 5" `Quick (fun () ->
              check int "fact 5" 120 (Fact.test_fact_real 5));
          test_case "fact 6" `Quick (fun () ->
              check int "fact 6" 720 (Fact.test_fact_real 6));
          test_case "fact 7" `Quick (fun () ->
              check int "fact 7" 5040 (Fact.test_fact_real 7));
          test_case "fact 8" `Quick (fun () ->
              check int "fact 8" 40320 (Fact.test_fact_real 8));
          test_case "fact 9" `Quick (fun () ->
              check int "fact 9" 362880 (Fact.test_fact_real 9));
          test_case "fact 10" `Quick (fun () ->
              check int "fact 10" 3628800 (Fact.test_fact_real 10));
        ] );
      ( "fib_fun_pointer_vm",
        [
          test_case "fib 0" `Quick (fun () ->
              check int "fib 0" 1 (Fib.test_fib_real 0));
          test_case "fib 1" `Quick (fun () ->
              check int "fib 1" 1 (Fib.test_fib_real 1));
          test_case "fib 2" `Quick (fun () ->
              check int "fib 2" 2 (Fib.test_fib_real 2));
          test_case "fib 3" `Quick (fun () ->
              check int "fib 3" 3 (Fib.test_fib_real 3));
          test_case "fib 4" `Quick (fun () ->
              check int "fib 4" 5 (Fib.test_fib_real 4));
          test_case "fib 5" `Quick (fun () ->
              check int "fib 5" 8 (Fib.test_fib_real 5));
          test_case "fib 6" `Quick (fun () ->
              check int "fib 6" 13 (Fib.test_fib_real 6));
          test_case "fib 7" `Quick (fun () ->
              check int "fib 7" 21 (Fib.test_fib_real 7));
          test_case "fib 8" `Quick (fun () ->
              check int "fib 8" 34 (Fib.test_fib_real 8));
          test_case "fib 9" `Quick (fun () ->
              check int "fib 9" 55 (Fib.test_fib_real 9));
          test_case "fib 10" `Quick (fun () ->
              check int "fib 10" 89 (Fib.test_fib_real 10));
        ] );
      ( "fib_stack_vm",
        [
          test_case "fib 0" `Quick (fun () ->
              check int "fib 0" 1 (Fib.test_fib 0));
          test_case "fib 1" `Quick (fun () ->
              check int "fib 1" 1 (Fib.test_fib 1));
          test_case "fib 2" `Quick (fun () ->
              check int "fib 2" 2 (Fib.test_fib 2));
          test_case "fib 3" `Quick (fun () ->
              check int "fib 3" 3 (Fib.test_fib 3));
          test_case "fib 4" `Quick (fun () ->
              check int "fib 4" 5 (Fib.test_fib 4));
          test_case "fib 5" `Quick (fun () ->
              check int "fib 5" 8 (Fib.test_fib 5));
          test_case "fib 6" `Quick (fun () ->
              check int "fib 6" 13 (Fib.test_fib 6));
          test_case "fib 7" `Quick (fun () ->
              check int "fib 7" 21 (Fib.test_fib 7));
          test_case "fib 8" `Quick (fun () ->
              check int "fib 8" 34 (Fib.test_fib 8));
          test_case "fib 9" `Quick (fun () ->
              check int "fib 9" 55 (Fib.test_fib 9));
          test_case "fib 10" `Quick (fun () ->
              check int "fib 10" 89 (Fib.test_fib 10));
        ] );
    ]
