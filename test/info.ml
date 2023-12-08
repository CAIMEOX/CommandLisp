open Exp
open Command_lisp
open Compile

let cmp =
  let open Casm in
  fact 20 |> preprocess_and_compile |> label_after_call

let ele =
  let funs = cmp |> List.rev |> Casm.collect_labels in
  let f' =
    List.map (fun (l, instrs) -> (l, Casm.dead_code_elimination instrs (List.map fst funs))) funs
  in
  Casm.numberize_labels f'

let t1 = Arch.compile_fntbale ele

let count_of_t1 =
  List.fold_left (fun a (_, l) -> a + List.length (String.split_on_char '\n' l)) 0 t1

let t1' () = Arch.file_save t1 "functions"
let t2 () = Fmt.print_labeled ele
