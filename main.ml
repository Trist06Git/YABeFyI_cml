(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

let bf_machine = new Machine.machine in

let rec loop str brs n bf_machine =
  if n = String.length str then
    Machine.Fin
  else begin
    let c = (String.get str n) in
    let res = (match c with
      | '>' -> bf_machine#inc_pc 0
      | '<' -> bf_machine#dec_pc 0
      | '+' -> bf_machine#inc_reg 0
      | '-' -> bf_machine#dec_reg 0
      | '.' -> bf_machine#get 0
      | ',' -> bf_machine#put (Char.code (input_char stdin)) (*Char.code (mon_in#get () )*)
      | '[' -> (if bf_machine#reg_val = 0 then begin
                 match Br_check.get_right n brs with
                   | Some x -> (loop str brs (x + 1) bf_machine)
                   | _      -> Machine.Fin
               end
               else
                 Machine.Loop bf_machine#reg_val)
      | ']' -> (match Br_check.get_left n brs with
                  | Some x -> (loop str brs x bf_machine)
                  | _      -> Machine.Fin
               )
      |  _  -> Machine.Non
    ) in

    (*(*for debug*)
    (match res with
      | Machine.Output x -> Printf.printf "pos %i was output of %i : %c" n x (char_of_int x)
      | Machine.Action x -> Printf.printf "pos %i was action of %i" n x
      | Machine.Input  x -> Printf.printf "pos %i was action of %i" n x
      | Machine.Loop   x -> Printf.printf "pos %i was loop   of %i" n x
      | Machine.Fin -> Printf.printf "\ndone\n" ; exit 0
      | Machine.Non -> Printf.printf "pos %i unknown char   " n
      (*| _ -> ()*) ) ;
    Printf.printf " : %c" c ;
    Printf.printf "        ptr_loc : %i\n" bf_machine#pc_val ;
    *)

    (match res with
      | Machine.Output x -> Printf.printf "%c" (char_of_int x)
      | Machine.Fin -> exit 0
      | _ -> () )
    ;
    
    loop str brs (n+1) bf_machine
  end
in

let main _ =
  if Array.length Sys.argv < 2 then
    Printf.printf "Error, no input file\n"
  else
    let str_in = (File_io.read_file_str Sys.argv.(1)) in
    let all_br = Br_check.proc_br str_in in

    match all_br with
      | Br_check.Error -> Printf.printf "Error, bad brackets!\nnow exiting\n" ; exit 1
      | Br_check.Ok x -> let all_br = x
    in
    ignore (loop str_in all_br 0 bf_machine)
in main 0
