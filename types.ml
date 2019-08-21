type command =
  | Inc
  | Dec
  | Left
  | Right
  | Print
  | Input
  | Loop of command list
  | Dump

type result =
  | Non
  | Fin
  | Output of int
  | Input of int
  | Action of int
  | Loop of int

type direction =
  | Left
  | Right

let cmd_to_string cmd =
  match cmd with
    | Inc   -> "Inc"
    | Dec   -> "Dec"
    | Left  -> "Left"
    | Right -> "Right"
    | Print -> "Print"
    | _     -> ""
