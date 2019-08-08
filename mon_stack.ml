(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

class ['a] mon_stack =
  object (self)
    val mutable ptr = (0 : int)
    val mutable stack = ([] : 'a list)

    method pull () =
      match stack with
        | [] -> None
        | h :: t -> (stack <- t ; Some h)

    method push x =
      stack <- [x] @ stack

    method is_empty () =
      match stack with
        | [] -> true
        | _  -> false

    method clear () =
      stack <- []

    method dump () =
      stack

  end
