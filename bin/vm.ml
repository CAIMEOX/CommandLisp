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
    ("Init VM with " ^ string_of_int (Array.length code) ^ " instructions:");
  print_endline "====Code Begin====";
  print_code_with_lines_aligned code;
  print_endline "=====Code End=====";
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
    (* | Ret n ->
           let r = pop vm in
           vm.sp <- vm.sp - n;
           let next_pc = pop vm in
           push vm r;
           vm.pc <- next_pc
       | Call (f, n) ->
           let next_pc = get_label vm.code f in
           vm.stack <- insert_at_index vm.stack (vm.sp - n) vm.pc;
           vm.sp <- vm.sp + 1;
           vm.pc <- next_pc *)
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
