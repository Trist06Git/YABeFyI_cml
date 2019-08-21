(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

type pr_res =
  | Error
  | Ok of (int*int) list

let proc_br str =
  let rec proc_br str n brs stack =
    if n = String.length str then begin
      if Stack.empty stack then 
        Ok brs
      else
        Error
    end
    else begin
      let c = String.get str n in
      if c = '[' then begin
        proc_br str (n+1) brs (Stack.push stack n)
      end
      else if c = ']' then begin
        let res = Stack.pull stack in
        match res with
          | (None, _) -> Error
          | (Some top, stack) -> proc_br str (n+1) (brs @ [(top, n)]) stack
      end
      else proc_br str (n+1) brs stack
    end
  in proc_br str 0 [] (Stack.make ())

let rec get_oposite x lst this other =
  if lst = [] then
    None
  else if x = this (List.hd lst) then
    Some (other (List.hd lst))
  else
    get_oposite x (List.tl lst) this other

let get_left r lst =
  get_oposite r lst snd fst

let get_right l lst = 
  get_oposite l lst fst snd

let dump_brs x =
  match x with
    | Error -> Printf.printf "Error, bad brackets!"
    | Ok y  -> let x = y in
  let rec print_br brs =
    if brs = [] then
      ()
    else
      let this_br = List.hd brs in
      Printf.printf "%i : %i\n" (fst this_br) (snd this_br) ;
      print_br (List.tl brs)
  in print_br x
