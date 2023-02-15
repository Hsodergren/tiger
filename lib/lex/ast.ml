type t =
  | Empty
  | Let of t list * t
  | Var of string * string option * t
  | Type of string * string
