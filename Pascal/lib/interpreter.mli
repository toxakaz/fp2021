open Ast

type interpret_result =
  | Succes of world
  | Fail of exn

(** parse -> semantic test -> interpret *)
val interpret : string -> interpret_result
