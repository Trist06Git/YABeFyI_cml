(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

let parse str =
  if String.length str = 0 then Types.None else
  let rec parse str n brs =
    if n = String.length str then [] else

    let c = String.get str n in
    match c with
      | '>' -> Types.Right  :: (parse str (n+1) brs)
      | '<' -> Types.Left   :: (parse str (n+1) brs)
      | '+' -> Types.Inc    :: (parse str (n+1) brs)
      | '-' -> Types.Dec    :: (parse str (n+1) brs)
      | ',' -> Types.Input  :: (parse str (n+1) brs)
      | '.' -> Types.Output :: (parse str (n+1) brs)
      | '[' -> (let op = (Br_check.get_oposite n brs fst snd) in
                match op with
                  | None -> []
                  | Some x -> let op = x in
                (Types.Loop (parse str (n+1) brs)) :: (parse str (op+1) brs)
               )
      | ']' -> []
      |  _  -> parse str (n+1) brs
  in
  let res = Br_check.proc_br str in
  match res with
    | Br_check.Ok x  -> Types.Some (parse str 0 x)
    | Br_check.Error -> Types.Error
  
