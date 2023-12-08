open Compile.Casm

type vm = {
  code : instr array;
  mutable stack : int array;
  call_stack : int array;
  mutable csp : int;
  mutable pc : int;
  mutable sp : int;
}

let initVm code =
  { code; stack = Array.make 100 0; call_stack = Array.make 30 0; csp = 0; pc = 0; sp = 0 }

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
  pop vm

module RealVm = struct
  type registers = {
    x : int array; [@length 10]
    a : int array; [@length 10]
    t : int array; [@length 10]
  }

  type vm = {
    funs : Compile.Casm.string_list_map;
    mutable stack : int array;
    call_stack : int array;
    registers : registers;
    mutable fp : string;
    mutable csp : int;
    mutable sp : int;
  }

  let initVm map =
    {
      funs = map;
      stack = Array.make 100 0;
      call_stack = Array.make 30 0;
      fp = "0";
      registers = { x = Array.make 10 0; a = Array.make 10 0; t = Array.make 10 0 };
      csp = 0;
      sp = 0;
    }

  let pop_to_reg vm regi =
    vm.sp <- vm.sp - 1;
    vm.registers.x.(regi) <- vm.stack.(vm.sp)

  let push_from_reg vm regi =
    vm.stack.(vm.sp) <- vm.registers.x.(regi);
    vm.sp <- vm.sp + 1

  let perf_add vm reg1 reg2 = vm.registers.x.(reg1) <- vm.registers.x.(reg1) + vm.registers.x.(reg2)
  let perf_sub vm reg1 reg2 = vm.registers.x.(reg1) <- vm.registers.x.(reg1) - vm.registers.x.(reg2)
  let perf_mul vm reg1 reg2 = vm.registers.x.(reg1) <- vm.registers.x.(reg1) * vm.registers.x.(reg2)
  let perf_div vm reg1 reg2 = vm.registers.x.(reg1) <- vm.registers.x.(reg1) / vm.registers.x.(reg2)
  let load_to_reg vm regi v = vm.registers.x.(regi) <- v
  let read_from_reg vm regi = vm.registers.x.(regi)

  let push_ret vm v =
    vm.call_stack.(vm.csp) <- v;
    vm.csp <- vm.csp + 1

  let pop_ret vm =
    vm.csp <- vm.csp - 1;
    vm.call_stack.(vm.csp)

  let rec run_batch vm set =
    let fp = ref vm.fp in
    let rec aux vm instrs =
      match instrs with
      | [] -> ()
      | instr :: rest ->
          run_single vm instr;
          if vm.fp = "exited" then pop_to_reg vm 0
          else if vm.fp <> !fp then (
            print_endline ("Switching from " ^ !fp ^ " to " ^ vm.fp);
            fp := vm.fp;
            aux vm (Hashtbl.find vm.funs vm.fp))
          else aux vm rest
    in
    aux vm set

  and run_single vm i =
    match i with
    | Cst i ->
        load_to_reg vm 0 i;
        push_from_reg vm 0
    | Add ->
        pop_to_reg vm 0;
        pop_to_reg vm 1;
        perf_add vm 0 1;
        push_from_reg vm 0
    | Sub ->
        pop_to_reg vm 0;
        pop_to_reg vm 1;
        perf_sub vm 0 1;
        push_from_reg vm 0
    | Mul ->
        pop_to_reg vm 0;
        pop_to_reg vm 1;
        perf_mul vm 0 1;
        push_from_reg vm 0
    | Div ->
        pop_to_reg vm 0;
        pop_to_reg vm 1;
        perf_div vm 0 1;
        push_from_reg vm 0
    | Var i ->
        load_to_reg vm 0 vm.stack.(vm.sp - i - 1);
        push_from_reg vm 0
    | Pop -> pop_to_reg vm 0
    | Swap ->
        pop_to_reg vm 0;
        pop_to_reg vm 1;
        push_from_reg vm 0;
        push_from_reg vm 1
    | Goto x -> vm.fp <- x
    | Ret n ->
        pop_to_reg vm 0;
        vm.sp <- vm.sp - n;
        push_from_reg vm 0;
        vm.fp <- vm |> pop_ret |> string_of_int
    | Call (f, n) ->
        push_ret vm n;
        vm.fp <- f
    | IfZero t ->
        pop_to_reg vm 0;
        if read_from_reg vm 0 = 0 then vm.fp <- t
    | Exit -> vm.fp <- "exited"
    | _ -> failwith "Not implemented"

  let run_with_switching vm =
    run_batch vm (Hashtbl.find vm.funs vm.fp);
    read_from_reg vm 0

  let run_real vm =
    let counter = ref 0 in
    let rec run vm cur_set =
      match cur_set with
      | [] -> failwith "unreachable"
      | instr :: rest -> (
          counter := !counter + 1;
          match instr with
          | Cst i ->
              load_to_reg vm 0 i;
              push_from_reg vm 0;
              run vm rest
          | Add ->
              pop_to_reg vm 0;
              pop_to_reg vm 1;
              perf_add vm 0 1;
              push_from_reg vm 0;
              run vm rest
          | Sub ->
              pop_to_reg vm 0;
              pop_to_reg vm 1;
              perf_sub vm 0 1;
              push_from_reg vm 0;
              run vm rest
          | Mul ->
              pop_to_reg vm 0;
              pop_to_reg vm 1;
              perf_mul vm 0 1;
              push_from_reg vm 0;
              run vm rest
          | Div ->
              pop_to_reg vm 0;
              pop_to_reg vm 1;
              perf_div vm 0 1;
              push_from_reg vm 0;
              run vm rest
          | Var i ->
              load_to_reg vm 0 vm.stack.(vm.sp - i - 1);
              push_from_reg vm 0;
              run vm rest
          | Pop ->
              pop_to_reg vm 0;
              run vm rest
          | Swap ->
              pop_to_reg vm 0;
              pop_to_reg vm 1;
              push_from_reg vm 0;
              push_from_reg vm 1;
              run vm rest
          | Goto x ->
              vm.fp <- x;
              run vm (Hashtbl.find vm.funs x)
          | Ret n ->
              pop_to_reg vm 0;
              vm.sp <- vm.sp - n;
              push_from_reg vm 0;
              vm.fp <- vm |> pop_ret |> string_of_int;
              run vm (Hashtbl.find vm.funs vm.fp)
          | Call (f, n) ->
              push_ret vm n;
              vm.fp <- f;
              run vm (Hashtbl.find vm.funs f)
          | IfZero t ->
              pop_to_reg vm 0;
              if read_from_reg vm 0 = 0 then (
                vm.fp <- t;
                run vm (Hashtbl.find vm.funs t))
              else run vm rest
          | Exit ->
              pop_to_reg vm 0;
              read_from_reg vm 0
          | _ -> failwith "Not implemented")
    in
    run vm (Hashtbl.find vm.funs vm.fp)

  let compile_and_run code =
    let funs = Compile.Casm.compile_to_function code in
    let vm = initVm funs in
    run_with_switching vm
end
