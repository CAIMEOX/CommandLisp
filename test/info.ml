class type textualizable = object
  method toString : string
end

(* this corresponds in Haskell to a multiparameter type class *)
class type ['b] printable = object ('a)
  constraint 'b = #textualizable
  method printWithPrefix : 'b -> unit
end

class textile (str_init : string) : textualizable =
  object
    val str = str_init
    method toString = str
  end

class intstring (i : int) : textualizable =
  object
    method toString = string_of_int i
  end

let do_print (it : #textualizable) = print_endline it#toString

let () =
  let it = new textile "hello" in
  do_print it;
  do_print (new intstring 42)

let print_myList (prefix : 'a) (li : 'a #printable list) =
  let print_it it = it#printWithPrefix prefix in
  print_endline "\nlist: ";
  List.iter print_it li

type txt = C of textualizable

let print_txt (C it) = print_endline it#toString
