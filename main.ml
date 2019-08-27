(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

open Types

let create_mem n =
  if n < 0 then [] else
  let rec create_mem n lst =
    match n with
      | 0 -> lst
      | _ -> create_mem (n-1) (0 :: lst)
  in create_mem n []

let rec traverse cmds left right debug =
  if debug then (match cmds with(*for debug*)
    | [] -> ()
    | h :: _ -> Printf.printf "cmd : %s\n" (cmd_to_string h)
  ) ;
  match cmds with
    | []          -> Types.Fin
    | Right  :: t -> (match right with
                        | []         -> traverse t left right debug
                        | r_h :: r_t -> traverse t (r_h :: left) r_t debug
                     )
    | Left   :: t -> (match left with
                        |  _  :: []   -> traverse t left right debug
                        | l_h :: l_t  -> traverse t l_t (l_h :: right) debug
                        | []          -> Types.Non
                     )
    | Inc    :: t -> let value = if (List.hd left) = 255 then 0 else ((List.hd left) + 1) in
                     (traverse t (value :: List.tl left) right) debug
    | Dec    :: t -> let value = if (List.hd left) = 0 then 255 else ((List.hd left) - 1) in
                     (traverse t (value :: List.tl left) right) debug
    | Output :: t -> (print_char (Char.chr (List.hd left)) ; traverse t left right) debug
    | Input  :: t -> (match left with
                        | []       -> Types.Fin
                        | _ :: l_t -> traverse t ((Char.code (input_char stdin)) :: l_t) right debug
                     )
    | Loop x :: t -> (match left with
                        | 0 :: _ -> traverse t left right debug
                        | _      -> traverse (x @ cmds) left right debug
                     )

let _ =
  if Array.length Sys.argv < 2 then
    Printf.printf "Error, no input file\n"
  else
    let str_in = (File_io.read_file_str Sys.argv.(1)) in
    let debug = if Array.length Sys.argv > 2 && Sys.argv.(2) = "debug" then true else false in

    match Parse.parse str_in with
      | Some prog -> (if debug then dump_cmds prog ; ignore (traverse prog [0] (create_mem 30000) debug) )
      | Error     -> Printf.printf "Error, bad brackets!\nnow exiting\n"
      | None      -> Printf.printf "Empty program file\nnow exiting\n"
