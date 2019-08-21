(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

let create_mem n =
  if n < 0 then [] else
  let rec create_mem n lst =
    match n with
      | 0 -> lst
      | _ -> create_mem (n-1) (0 :: lst)
  in create_mem n []
in

let rec loop str brs n left right =
  if n = String.length str then
    Types.Fin
  else begin
    let c = (String.get str n) in
    match c with
      | '>' -> (match right with
                  | []     -> loop str brs (n+1) left right
                  | h :: t -> loop str brs (n+1) (h :: left) t
               )
      | '<' -> (match left with
                  | _ :: [] -> loop str brs (n+1) left right
                  | h :: t  -> loop str brs (n+1) t (h :: right)
                  | []      -> Types.Non
               )
      | '+' -> (loop str brs (n+1) (((List.hd left) + 1) :: List.tl left) right)
      | '-' -> (loop str brs (n+1) (((List.hd left) - 1) :: List.tl left) right)
      | '.' -> (print_char (Char.chr (List.hd left)) ; loop str brs (n+1) left right)
      | ',' -> (match left with
                  | []     -> Types.Fin
                  | _ :: t -> (loop str brs (n+1) ((Char.code (input_char stdin)) :: t) right)
               )
      | '[' -> (if List.hd left = 0 then begin
                 match Br_check.get_right n brs with
                   | Some x -> (loop str brs (x + 1) left right)
                   | _      -> Types.Fin
               end
               else
                 loop str brs (n+1) left right)
      | ']' -> (match Br_check.get_left n brs with
                  | Some x -> (loop str brs x left right)
                  | _      -> Types.Fin
               )
      |  _  -> loop str brs (n+1) left right
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
      | Br_check.Ok x  -> let all_br = x
    in
    ignore (loop str_in all_br 0 [0] (create_mem 3000))
in main 0
