type name = string

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | FDiv
  | And
  | Or
  | Xor
  | Greater
  | Less
  | Eq
  | NotEq
  | GreaterEq
  | LessEq
  | RShift
  | LShift

type unop =
  | Plus
  | Minus
  | Not

type vtype =
  | VTBool
  | VTInt
  | VTFloat
  | VTChar
  | VTString of expr
  | VTNDString
  | VTDString of int
  | VTRecord of (name * vtype) list
  | VTFunction of fun_param list * vtype
  | VTArray of expr * expr * vtype
  | VTDArray of value * value * vtype
  | VTCustom of name
  | VTVoid

and value =
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VChar of char
  | VString of string
  | VRecord of (name * value) list
  | VFunction of name * vtype * fun_param list * t
  | VArray of value * value list
  | VVoid

and fun_param =
  | FPFree of name * vtype
  | FPOut of name * vtype
  | FPConst of name * vtype

and expr =
  | Const of value
  | Variable of name
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Call of name * expr list
  | GetRec of expr * name
  | GetArr of expr * expr

and statement =
  | Assign of expr * expr
  | ProcCall of expr
  | If of expr * statement list * statement list
  | While of expr * statement list
  | Repeat of expr * statement list
  | For of name * expr * expr * statement list
  | Break
  | Continue

and define =
  | DType of name * vtype
  | DNDVariable of name * vtype
  | DVariable of name * vtype * expr
  | DDVariable of name * vtype * value
  | DConst of name * expr
  | DDConst of name * value
  | DFunction of name * vtype * fun_param list * t

and t = define list * statement list
