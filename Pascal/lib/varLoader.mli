open Ast

(** Create world from def list.
    Fills created variables with std values.*)
val load_variables : define list -> world
