open Ast

(** Get value type. *)
val get_type_val : value -> vtype

(** Compare types except types with expr in definition. *)
val compare_types : vtype -> vtype -> bool
