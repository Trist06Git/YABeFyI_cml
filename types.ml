(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

type cmd =
  | Inc
  | Dec
  | Left
  | Right
  | Input
  | Output
  | Loop of cmd list

type cmd_opt =
  | Some of cmd list
  | None
  | Error

type result =
  | Non
  | Fin
  | Output of int
  | Input of int
  | Action of int
  | Loop of int

let cmd_to_string cmd =
  match cmd with
    | Inc    -> "Inc"
    | Dec    -> "Dec"
    | Left   -> "Left"
    | Right  -> "Right"
    | Output -> "Output"
    | Input  -> "Input"
    | Loop _ -> "[Loop"

let dump_cmds cmds =
  let rec dump_cmds cmds =
    match cmds with
      | []     ->  Printf.printf "end] "
      | h :: t -> (Printf.printf "%s; " (cmd_to_string h) ;
                  match h with
                    | Loop x -> (dump_cmds x ; dump_cmds t)
                    | _      -> dump_cmds t
                  )
  in Printf.printf "program tree :\n" ; dump_cmds cmds ; Printf.printf "\n\n"
  
