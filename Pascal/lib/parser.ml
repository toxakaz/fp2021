open Opal
open Ast

let ( let* ) = ( >>= )
let ( &> ) p f = p >>= fun r -> return (f r)
let cl2s cl = String.concat "" (List.map (String.make 1) cl)

let use_parser parser input_string =
  parser (LazyStream.of_string (String.lowercase_ascii input_string))
;;

let check_parser parser input_string expected =
  let empty_input = LazyStream.Nil in
  match use_parser parser input_string with
  | Some (x, l) -> x = expected && l = empty_input
  | _ -> false
;;

let check_parser_fail parser input_string =
  let empty_input = LazyStream.Nil in
  match use_parser parser input_string with
  | None -> true
  | Some (_, l) when l != empty_input -> true
  | _ -> false
;;

let key_words =
  List.map
    (fun s -> token s)
    (List.sort
       (fun s1 s2 -> String.compare s2 s1)
       [ "const" (*definition key words*)
       ; "type"
       ; "var"
       ; "integer"
       ; "boolean"
       ; "float"
       ; "char"
       ; "string"
       ; "record"
       ; "array"
       ; "function"
       ; "procedure"
       ; "out" (*boolean key words*)
       ; "true"
       ; "false" (*statement key words*)
       ; "begin"
       ; "end"
       ; "if"
       ; "then"
       ; "else"
       ; "while"
       ; "repeat"
       ; "for"
       ; "do"
       ; "until"
       ; "in"
       ; "to"
       ; "downto"
       ; "continue"
       ; "break"
       ])
;;

let key_word = choice key_words

let%test "key_word begin" = check_parser key_word "begin" "begin"
let%test "key_word integer" = check_parser key_word "integer" "integer"

let word =
  let* first = lexeme (letter <|> exactly '_') in
  let* other = many (letter <|> exactly '_' <|> digit) in
  let result = cl2s (first :: other) in
  match use_parser key_word result with
  | None -> return result
  | _ -> mzero
;;

let value =
  let sign = option '+' (exactly '-') in
  let integer =
    let* sign = sign in
    let* digits = many1 digit in
    return (int_of_string (cl2s (sign :: digits)))
  in
  let float =
    let* sign = sign in
    let* int_part = many1 digit in
    let* float_part = exactly '.' >> many digit in
    return (float_of_string (cl2s (sign :: (int_part @ ('.' :: float_part)))))
  in
  let symbol =
    any
    >>= function
    | '\\' -> any
    | '\'' -> mzero
    | c -> return c
  in
  let char =
    let ascii =
      let* ord = exactly '@' >> many1 digit in
      return (Char.chr (int_of_string (cl2s ord)))
    in
    between (exactly '\'') (exactly '\'') (ascii <|> symbol)
  in
  let string = between (exactly '\'') (exactly '\'') (many1 symbol) &> cl2s in
  let bool =
    key_word
    >>= function
    | "true" -> return true
    | "false" -> return false
    | _ -> mzero
  in
  spaces
  >> choice
       [ (float &> fun r -> VFloat r)
       ; (integer &> fun r -> VInt r)
       ; (char &> fun r -> VChar r)
       ; (string &> fun r -> VString r)
       ; (bool &> fun r -> VBool r)
       ]
;;

let%test "value int -42" = check_parser value "-42" (VInt (-42))
let%test "value int 128" = check_parser value "128" (VInt 128)
let%test "value float -42.128" = check_parser value "-42.128" (VFloat (-42.128))
let%test "value float 42." = check_parser value "42." (VFloat 42.)
let%test "value char c" = check_parser value "\'c\'" (VChar 'c')
let%test "value char ascii 65 (A)" = check_parser value "\'@65\'" (VChar (Char.chr 65))
let%test "value char quote" = check_parser value "\'\\\'\'" (VChar '\'')
let%test "value char slash" = check_parser value "\'\\\\\'" (VChar '\\')
let%test "value string" = check_parser value "\'foo \\\'bar\\\'\'" (VString "foo \'bar\'")
let%test "value empty quotes" = check_parser_fail value "\'\'"
let%test "value bool true" = check_parser value "true" (VBool true)
let%test "value bool false" = check_parser value "false" (VBool false)

(*list of lists of binary operator parsers, placed by priority*)
let binop =
  List.map
    (List.map (fun (op, s) -> token s &> fun _ -> op))
    [ [ Mul, "*"
      ; FDiv, "/"
      ; Div, "div"
      ; Mod, "mod"
      ; And, "and"
      ; RShift, "shr"
      ; LShift, "shl"
      ; RShift, ">>"
      ; LShift, "<<"
      ]
    ; [ Add, "+"; Sub, "-"; Or, "or"; Xor, "xor" ]
    ; [ Eq, "="; NotEq, "<>"; Less, "<"; LessEq, "<="; Greater, ">"; GreaterEq, ">=" ]
    ]
;;

let unop =
  List.map
    (List.map (fun (op, s) -> token s &> fun _ -> op))
    [ [ Plus, "+"; Minus, "-"; Not, "not" ] ]
;;

let rec expr s =
  let parens p = between (token "(") (token ")") p in
  let func =
    let* fname = word in
    let* params = parens (sep_by expr (token ",")) in
    return (Call (fname, params))
  in
  let const = value &> fun r -> Const r in
  let variable = word &> fun r -> Variable r in
  let factor = choice [ func; variable; const; parens expr ] in
  let unpak =
    let rec helper obj =
      let rec_unpack =
        let* field = token "." >> word in
        return (GetRec (obj, field))
      in
      let arr_unpack =
        let* ind = between (token "[") (token "]") (sep_by1 expr (token ",")) in
        return (List.fold_left (fun exp i -> GetArr (exp, i)) obj ind)
      in
      option obj (rec_unpack <|> arr_unpack >>= helper)
    in
    factor >>= helper
  in
  let parse_unop =
    let use_unop greater curr =
      let* ops = many (choice curr) in
      let* obj = greater in
      return (List.fold_left (fun exp op -> UnOp (op, exp)) obj ops)
    in
    List.fold_left use_unop unpak unop
  in
  let parse_binop =
    let use_binop greater curr =
      let* start = greater in
      let op_term =
        let* op = choice curr in
        let* term = greater in
        return (op, term)
      in
      let* ops = many op_term in
      return (List.fold_left (fun left (op, right) -> BinOp (op, left, right)) start ops)
    in
    List.fold_left use_binop parse_unop binop
  in
  parse_binop s
;;

let%test "a + b - c" =
  check_parser
    expr
    "a + b - c"
    (BinOp (Sub, BinOp (Add, Variable "a", Variable "b"), Variable "c"))
;;

let%test "a + - b * c" =
  check_parser
    expr
    "a + - b * c"
    (BinOp (Add, Variable "a", BinOp (Mul, UnOp (Minus, Variable "b"), Variable "c")))
;;

let%test "(a[1, 2][3] shl b) * c" =
  check_parser
    expr
    "(a[1, 2][3] shl b) * c"
    (BinOp
       ( Mul
       , BinOp
           ( LShift
           , GetArr
               ( GetArr (GetArr (Variable "a", Const (VInt 1)), Const (VInt 2))
               , Const (VInt 3) )
           , Variable "b" )
       , Variable "c" ))
;;

let%test " - func(a . b.c, 3) [ c ]" =
  check_parser
    expr
    " - func(a . b.c, 3) [ c ]"
    (UnOp
       ( Minus
       , GetArr
           ( Call ("func", [ GetRec (GetRec (Variable "a", "b"), "c"); Const (VInt 3) ])
           , Variable "c" ) ))
;;

let parse _ = [], []
