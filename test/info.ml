open Exp
open Command_lisp
open Compile

let cmp =
  let open Casm in
  fib 20 |> preprocess_and_compile |> label_after_call

let ele =
  let funs = cmp |> List.rev |> Casm.collect_labels in
  let f' =
    List.map
      (fun (l, instrs) ->
        (l, Casm.dead_code_elimination instrs (List.map fst funs)))
      funs
  in
  Casm.numberize_labels f'

let t2 () = Fmt.print_labeled ele
