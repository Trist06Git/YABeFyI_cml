(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

type 'a stack = 'a list

let make () =
  ([]:'a stack)

let push (s:'a stack) a =
  a :: s

let pull (s:'a stack) =
  match s with
    | h :: t -> (Some h, t)
    | []     -> (None, [])

let peek (s:'a stack) =
  match s with
    | h :: _ -> Some h
    | []     -> None

let size (s:'a stack) =
  List.length s

let empty (s:'a stack) =
  match s with
    | [] -> true
    | _  -> false

let dump _ =
  Printf.printf "stub\n"

(*
let rec dump (s:stack) =
  match s with
    | h :: t -> Printf.printf "%i;" h ; dump t
    | []     -> Printf.printf "\n"
*)