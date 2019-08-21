(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

let read_file filename =
  let lines = ref [] in
  let chan = 
      (try
        Some (open_in filename)
      with
        | _ -> None)
  in
  match chan with
    | None   -> print_endline "Open File Error!" ; []
    | Some x -> let chan = x in
    try
      while true do
        lines := input_line chan :: !lines
      done; !lines
    with
      | End_of_file -> ( close_in chan ; List.rev !lines )
      | _           -> ( close_in chan ; List.rev !lines )

let lines_to_string lst =
  String.concat "" lst

let read_file_str filename =
  read_file filename |> lines_to_string
