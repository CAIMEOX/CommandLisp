open Arch

type vm = {
  code : instr array;
  mutable stack : int array;
  call_stack : int array;
  mutable csp : int;
  mutable pc : int;
  mutable sp : int;
}

let number_digit_len x =
  let rec loop x acc = if x < 10 then acc + 1 else loop (x / 10) (acc + 1) in
  loop x 0

let fmt_align_number n max =
  let s = string_of_int n in
  let len = String.length s in
  let padding = String.make (number_digit_len max - len) ' ' in
  padding ^ s

let print_code_with_lines_aligned code =
  let m = Array.length code in
  Array.iteri
    (fun i instr ->
      print_endline (fmt_align_number i m ^ " | " ^ string_of_instr instr))
    code

let initVm code =
  print_endline
    ("Init VM with "
    ^ string_of_int (Array.length code)
    ^ " instructions (source code):");
  {
    code;
    stack = Array.make 100 0;
    call_stack = Array.make 30 0;
    csp = 0;
    pc = 0;
    sp = 0;
  }

let get_label code label =
  let rec loop i =
    if i >= Array.length code then failwith "Label not found"
    else match code.(i) with Label l when l = label -> i | _ -> loop (i + 1)
  in
  loop 0

let insert_at_index arr i v =
  let len = Array.length arr in
  let new_arr = Array.make (len + 1) v in
  Array.blit arr 0 new_arr 0 i;
  Array.blit arr i new_arr (i + 1) (len - i);
  new_arr

let pop vm =
  vm.sp <- vm.sp - 1;
  vm.stack.(vm.sp)

let push vm v =
  vm.stack.(vm.sp) <- v;
  vm.sp <- vm.sp + 1

let push_ret vm v =
  vm.call_stack.(vm.csp) <- v;
  vm.csp <- vm.csp + 1

let pop_ret vm =
  vm.csp <- vm.csp - 1;
  vm.call_stack.(vm.csp)

let run vm =
  let counter = ref 0 in
  let break = ref false in
  while !break = false do
    counter := !counter + 1;
    let i = vm.code.(vm.pc) in
    (* print_endline (string_of_int !counter ^ ": " ^ string_of_instr i); *)
    vm.pc <- vm.pc + 1;
    match i with
    | Cst i -> push vm i
    | Add ->
        let a = pop vm in
        let b = pop vm in
        push vm (a + b)
    | Sub ->
        let a = pop vm in
        let b = pop vm in
        push vm (a - b)
    | Mul ->
        let a = pop vm in
        let b = pop vm in
        push vm (a * b)
    | Div ->
        let a = pop vm in
        let b = pop vm in
        push vm (a / b)
    | Var i -> push vm vm.stack.(vm.sp - i - 1)
    | Pop -> ignore (pop vm)
    | Swap ->
        let a = pop vm in
        let b = pop vm in
        push vm a;
        push vm b
    | Label _ -> ()
    | Ret n ->
        let r = pop vm in
        vm.sp <- vm.sp - n;
        push vm r;
        vm.pc <- pop_ret vm
    | Call (f, _) ->
        push_ret vm vm.pc;
        vm.pc <- get_label vm.code f
    | IfZero t ->
        let a = pop vm in
        if a = 0 then vm.pc <- get_label vm.code t
    | Goto t -> vm.pc <- get_label vm.code t
    | Exit -> break := true
    | _ -> failwith "Not implemented"
  done;
  print_endline ("Executed " ^ string_of_int !counter ^ " instructions");
  (* print vm stack *)
  pop vm

module RealVm = struct
  let counter = ref 0

  let call_counter prefix () =
    let n = !counter in
    counter := n + 1;
    "call_" ^ prefix ^ "_" ^ string_of_int n

  let collect_labels code =
    let rec loop index instrs res =
      if index >= Array.length code then res
      else
        match code.(index) with
        | Call (l, n) ->
            let new_label = call_counter l () in
            loop (index + 1) [ Call (new_label, -10) ] res
            @ [ (new_label, Call (l, n) :: instrs) ]
        | Label l -> loop (index + 1) [ Call (l, -1) ] res @ [ (l, instrs) ]
        | _ -> loop (index + 1) (code.(index) :: instrs) res
    in
    loop 0 [] []

  let compile_to_function code =
    let inversed = Array.of_list (List.rev code) in
    print_code_with_lines_aligned (Array.of_list code);
    let funs = collect_labels inversed in
    let map = Hashtbl.create (List.length funs) in
    List.iter (fun (l, instrs) -> Hashtbl.add map l instrs) funs;
    map

  let format_labeled tbl =
    List.map
      (fun (l, instrs) ->
        l ^ ": \n"
        ^ (List.map (fun t -> " " ^ string_of_instr t) instrs
          |> String.concat "\n"))
      tbl

  let print_labeled tbl = List.iter print_endline (format_labeled tbl)

  type string_list_map = (string, instr list) Hashtbl.t

  type vm = {
    funs : string_list_map;
    mutable stack : int array;
    call_stack : string array;
    mutable fp : label;
    mutable csp : int;
    mutable sp : int;
  }

  let initVm map =
    {
      funs = map;
      stack = Array.make 100 0;
      call_stack = Array.make 30 "";
      fp = "entry";
      csp = 0;
      sp = 0;
    }

  let pop vm =
    vm.sp <- vm.sp - 1;
    vm.stack.(vm.sp)

  let push vm v =
    vm.stack.(vm.sp) <- v;
    vm.sp <- vm.sp + 1

  let push_ret vm v =
    vm.call_stack.(vm.csp) <- v;
    vm.csp <- vm.csp + 1

  let pop_ret vm =
    vm.csp <- vm.csp - 1;
    vm.call_stack.(vm.csp)

  let rec run vm cur_set =
    match cur_set with
    | [] ->
        print_endline ("VM finished in " ^ vm.fp);
        pop vm
    | instr :: rest -> (
        print_endline (vm.fp ^ ": " ^ string_of_instr instr);
        match instr with
        | Cst i ->
            push vm i;
            run vm rest
        | Add ->
            let a = pop vm in
            let b = pop vm in
            push vm (a + b);
            run vm rest
        | Sub ->
            let a = pop vm in
            let b = pop vm in
            push vm (a - b);
            run vm rest
        | Mul ->
            let a = pop vm in
            let b = pop vm in
            push vm (a * b);
            run vm rest
        | Div ->
            let a = pop vm in
            let b = pop vm in
            push vm (a / b);
            run vm rest
        | Var i ->
            push vm vm.stack.(vm.sp - i - 1);
            run vm rest
        | Pop ->
            ignore (pop vm);
            run vm rest
        | Swap ->
            let a = pop vm in
            let b = pop vm in
            push vm a;
            push vm b;
            run vm rest
        | Label _ -> run vm rest
        | Ret n ->
            let r = pop vm in
            vm.sp <- vm.sp - n;
            push vm r;
            vm.fp <- pop_ret vm;
            run vm (Hashtbl.find vm.funs vm.fp)
        | Call (f, _) ->
            push_ret vm vm.fp;
            vm.fp <- f;
            run vm (Hashtbl.find vm.funs f)
        | IfZero t ->
            let a = pop vm in
            if a = 0 then (
              vm.fp <- t;
              run vm (Hashtbl.find vm.funs t))
            else run vm rest
        | Goto t ->
            vm.fp <- t;
            run vm (Hashtbl.find vm.funs t)
        | Exit -> pop vm
        | _ -> failwith "Not implemented")

  let first_run vm = run vm (Hashtbl.find vm.funs vm.fp)
end
