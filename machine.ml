(* 
Author  : Flesh_Bag, Trist06@hotmail.com
licence : GPLv2
*)

type result =
  | Non
  | Fin
  | Output of int
  | Input of int
  | Action of int
  | Loop of int

class machine = 
  object (self)
    val mutable mem = Array.make 3000 0
    val mutable pc = (0 : int)
    val bit_width = 8
    
    method pc_val = pc
    method reg_val = Array.get mem pc

    method inc_pc (_ : int) = 
      if pc = (Array.length mem) - 1 then Action pc else (pc <- pc + 1 ; Action pc)
    method dec_pc (_ : int) =
      if pc = 0 then Action pc else (pc <- pc - 1 ; Action pc)

    method inc_reg (_ : int) = 
      let n = (Array.get mem pc) + 1 in
      if n = Mon_maths.pow 2 bit_width then
        (Array.set mem pc 0 ; Action 0)
      else
        (Array.set mem pc n ; Action n)

    method dec_reg (_ : int) =
      let n = (Array.get mem pc) - 1 in
      if n < 0 then
        Array.set mem pc ((Mon_maths.pow 2 bit_width) - 1)
      else
        Array.set mem pc n
      ;
      Action (Array.get mem pc)

    method get (_ : int) =
      Output (Array.get mem pc)
    method put (x : int) =
      Array.set mem pc x ;
      Input (Array.get mem pc)
  
  end


