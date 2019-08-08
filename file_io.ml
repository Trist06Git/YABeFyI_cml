(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

let read_file_old filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do (*strangely the example had "true;", why?*)
      lines := input_line chan :: !lines
    done; !lines
  with
    (*| End_of_file -> close_in chan ;*)
    | _ -> close_in chan ; List.rev !lines

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

let mon_lines_to_string lst =
  let rec to_string lst str =
    if lst = [] then
      str
    else
      to_string (List.tl lst) (str ^ (List.hd lst))
  in to_string lst ""

let lines_to_string lst =
  String.concat "" lst

let read_file_str filename =
  read_file filename |> lines_to_string
