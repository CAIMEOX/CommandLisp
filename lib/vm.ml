open Arch

type vm = {
  code : instr array;
  mutable stack : int array;
  call_stack : int array;
  mutable csp : int;
  mutable pc : int;
  mutable sp : int;
}

let initVm code =
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
  let counter = ref 0
  let incr_counter () = counter := !counter + 1
  let refresh_counter () = counter := 0

  let call_counter prefix () =
    let n = !counter in
    "fp_next_call_" ^ prefix ^ "_" ^ string_of_int n

  let label_after_call instrs =
    let () = refresh_counter () in
    let rec loop rest res =
      match rest with
      | [] -> res
      | Call (l, _) :: rest ->
          let () = incr_counter () in
          let new_label = call_counter l () in
          loop rest (res @ [ Call (l, !counter); Label new_label ])
      | x :: rest -> loop rest (res @ [ x ])
    in
    loop instrs []

  let collect_labels instrs =
    let rec loop rest instrs res =
      match rest with
      | [] -> res
      | Label l :: rest -> loop rest [ Label l ] ([ (l, instrs) ] @ res)
      | x :: rest -> loop rest (x :: instrs) res
    in
    loop instrs [] []

  let compile_to_function instrs =
    let funs = instrs |> label_after_call |> List.rev |> collect_labels in
    let map = Hashtbl.create (List.length funs) in
    List.iter (fun (l, instrs) -> Hashtbl.add map l instrs) funs;
    map

  type string_list_map = (string, instr list) Hashtbl.t

  type registers = {
    x : int array; [@length 10]
    a : int array; [@length 10]
    t : int array; [@length 10]
  }

  type vm = {
    funs : string_list_map;
    mutable stack : int array;
    call_stack : string array;
    registers : registers;
    mutable fp : string;
    mutable csp : int;
    mutable sp : int;
  }

  let initVm map =
    {
      funs = map;
      stack = Array.make 100 0;
      call_stack = Array.make 30 "";
      fp = "entry";
      registers =
        { x = Array.make 10 0; a = Array.make 10 0; t = Array.make 10 0 };
      csp = 0;
      sp = 0;
    }

  let pop_to_reg vm regi =
    vm.sp <- vm.sp - 1;
    vm.registers.x.(regi) <- vm.stack.(vm.sp)

  let push_from_reg vm regi =
    vm.stack.(vm.sp) <- vm.registers.x.(regi);
    vm.sp <- vm.sp + 1

  let perf_add vm reg1 reg2 =
    vm.registers.x.(reg1) <- vm.registers.x.(reg1) + vm.registers.x.(reg2)

  let perf_sub vm reg1 reg2 =
    vm.registers.x.(reg1) <- vm.registers.x.(reg1) - vm.registers.x.(reg2)

  let perf_mul vm reg1 reg2 =
    vm.registers.x.(reg1) <- vm.registers.x.(reg1) * vm.registers.x.(reg2)

  let perf_div vm reg1 reg2 =
    vm.registers.x.(reg1) <- vm.registers.x.(reg1) / vm.registers.x.(reg2)

  let load_to_reg vm regi v = vm.registers.x.(regi) <- v
  let read_from_reg vm regi = vm.registers.x.(regi)

  let push_ret vm v =
    vm.call_stack.(vm.csp) <- v;
    vm.csp <- vm.csp + 1

  let pop_ret vm =
    vm.csp <- vm.csp - 1;
    vm.call_stack.(vm.csp)

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
          | Label x ->
              vm.fp <- x;
              run vm (Hashtbl.find vm.funs x)
          | Ret n ->
              pop_to_reg vm 0;
              vm.sp <- vm.sp - n;
              push_from_reg vm 0;
              vm.fp <- pop_ret vm;
              run vm (Hashtbl.find vm.funs vm.fp)
          | Call (f, _) -> (
              match rest with
              | Label next :: _ ->
                  push_ret vm next;
                  vm.fp <- f;
                  run vm (Hashtbl.find vm.funs f)
              | _ -> failwith "Call must be followed by a label")
          | IfZero t ->
              pop_to_reg vm 0;
              if read_from_reg vm 0 = 0 then (
                vm.fp <- t;
                run vm (Hashtbl.find vm.funs t))
              else run vm rest
          | Goto t ->
              vm.fp <- t;
              run vm (Hashtbl.find vm.funs t)
          | Exit ->
              pop_to_reg vm 0;
              read_from_reg vm 0
          | _ -> failwith "Not implemented")
    in
    run vm (Hashtbl.find vm.funs vm.fp)

  let compile_and_run code =
    let funs = compile_to_function code in
    let vm = initVm funs in
    run_real vm
end
