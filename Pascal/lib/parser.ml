(** Copyright 2021-2022, Kazancev Anton *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Opal
open Ast

let ( let* ) = ( >>= )
let ( &> ) p x = p => fun _ -> x
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

let word =
  let start = letter <|> exactly '_' in
  lexeme start <~> many (start <|> digit) => cl2s
;;

let%test "word usual" = check_parser word "someVariable_123" "somevariable_123"
let%test "word _ the first" = check_parser word "_someVariable" "_somevariable"
let%test "word digit the first" = check_parser_fail word "1_someVariable"

let token s =
  word
  <|> token s
  >>= function
  | x when x = s -> return s
  | _ -> mzero
;;

let%test "new token correct 1" = check_parser (token "+") "+" "+"
let%test "new token correct 2" = check_parser (token "sam") "sam" "sam"
let%test "new token fail" = check_parser_fail (token "sam") "samsung"

let check_next f s =
  match any s with
  | Some (x, _) when f (Some x) -> (return ()) s
  | None when f None -> (return ()) s
  | _ -> mzero s
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
       ; "real"
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
let%test "key_word not key_word" = check_parser_fail key_word "no_key_here"
let%test "key_word begins" = check_parser_fail key_word "begins"

let name =
  let* result = word in
  match use_parser key_word result with
  | None -> return result
  | _ -> mzero
;;

let%test "name usual" = check_parser name "someVariable_123" "somevariable_123"
let%test "name _ the first" = check_parser name "_someVariable" "_somevariable"
let%test "name digit the first" = check_parser_fail name "1_someVariable"
let%test "name when key_word given" = check_parser_fail name "begin"

let value =
  let integer = many1 digit => cl2s => int_of_string in
  let float =
    let* int_part = many1 digit in
    let* float_part =
      exactly '.'
      >> check_next (function
             | Some x -> x != '.'
             | _ -> true)
      >> many digit
    in
    return (float_of_string (cl2s (int_part @ ('.' :: float_part))))
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
  let string = between (exactly '\'') (exactly '\'') (many1 symbol) => cl2s in
  let bool =
    key_word
    >>= function
    | "true" -> return true
    | "false" -> return false
    | _ -> mzero
  in
  spaces
  >> choice
       [ (float => fun r -> VFloat r)
       ; (integer => fun r -> VInt r)
       ; (char => fun r -> VChar r)
       ; (string => fun r -> VString r)
       ; (bool => fun r -> VBool r)
       ]
;;

let%test "value int 128" = check_parser value "128" (VInt 128)
let%test "value float 42." = check_parser value "42." (VFloat 42.)
let%test "value float 42.." = check_parser_fail value "42.."
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
    (List.map (fun (op, s) -> token s &> op))
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
    (List.map (fun (op, s) -> token s &> op))
    [ [ Plus, "+"; Minus, "-"; Not, "not" ] ]
;;

let rec expr s =
  let parens p = between (token "(") (token ")") p in
  let func =
    let* f = many (satisfy (( != ) '(')) in
    let* params = parens (sep_by expr (token ",")) in
    match use_parser (expr << spaces) (cl2s f) with
    | Some (f, LazyStream.Nil) -> return (Call (f, params))
    | _ -> mzero
  in
  let const = value => fun r -> Const r in
  let variable = name => fun r -> Variable r in
  let factor = choice [ func; variable; const; parens expr ] in
  let unpak =
    let rec helper obj =
      let rec_unpack =
        let* field = token "." >> name in
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
           ( Call
               ( Variable "func"
               , [ GetRec (GetRec (Variable "a", "b"), "c"); Const (VInt 3) ] )
           , Variable "c" ) ))
;;

let rec statement s =
  let statement_block =
    between
      (token "begin")
      (many (token ";") >> token "end")
      (sep_by statement (many1 (token ";")))
    <|> (statement => fun st -> [ st ])
  in
  let assign_st =
    let* left = expr in
    let* right = token ":=" >> expr in
    return (Assign (left, right))
  in
  let proc_call_st = expr => fun x -> ProcCall x in
  let if_st =
    let* condition = token "if" >> expr in
    let* then_block = token "then" >> statement_block in
    let* else_block = option [] (token "else" >> statement_block) in
    return (If (condition, then_block, else_block))
  in
  let while_st =
    let* condition = token "while" >> expr in
    let* body = token "do" >> statement_block in
    return (While (condition, body))
  in
  let repeat_st =
    let* body =
      between
        (token "repeat")
        (many (token ";") >> token "until")
        (sep_by statement (many1 (token ";")))
    in
    let* condition = expr in
    return (Repeat (condition, body))
  in
  let for_st =
    let* control_variable = token "for" >> name in
    let* start = token ":=" >> expr in
    let* direction = token "to" &> true <|> (token "downto" &> false) in
    let* finish = expr in
    let* body = token "do" >> statement_block in
    if direction
    then return (For (control_variable, start, finish, body))
    else return (For (control_variable, finish, start, body))
  in
  let break_st = token "break" &> Break in
  let continue_st = token "continue" &> Continue in
  choice
    [ assign_st; if_st; while_st; repeat_st; for_st; break_st; continue_st; proc_call_st ]
    s
;;

let%test "assignment test" =
  check_parser
    statement
    "a.b := 42 + 5"
    (Assign (GetRec (Variable "a", "b"), BinOp (Add, Const (VInt 42), Const (VInt 5))))
;;

let%test "if _ then statement" =
  check_parser
    statement
    "if a then b:=10"
    (If (Variable "a", [ Assign (Variable "b", Const (VInt 10)) ], []))
;;

let%test "if _ then begend else begend" =
  check_parser
    statement
    "if a then begin b := 15; end else begin h := 8 end"
    (If
       ( Variable "a"
       , [ Assign (Variable "b", Const (VInt 15)) ]
       , [ Assign (Variable "h", Const (VInt 8)) ] ))
;;

let%test "repeat until" =
  check_parser
    statement
    "repeat a := a + 1; b := a - 1; until a < 128"
    (Repeat
       ( BinOp (Less, Variable "a", Const (VInt 128))
       , [ Assign (Variable "a", BinOp (Add, Variable "a", Const (VInt 1)))
         ; Assign (Variable "b", BinOp (Sub, Variable "a", Const (VInt 1)))
         ] ))
;;

let rec definition s =
  let names = sep_by name (token ",") in
  let compress_DType_list f lst =
    let res =
      List.fold_right
        (fun n acc ->
          match acc, n with
          | Some l, DNDVariable (n, tp) -> Some (f n tp :: l)
          | _ -> None)
        lst
        (Some [])
    in
    match res with
    | Some l -> return l
    | None -> mzero
  in
  let rec function_arg proc s =
    let helper =
      let fun_param =
        let helper f = as_var >>= compress_DType_list f in
        let fun_param_free = helper (fun n tp -> FPFree (n, tp)) in
        let fun_param_const = token "const" >> helper (fun n tp -> FPConst (n, tp)) in
        let fun_param_out = token "out" >> helper (fun n tp -> FPOut (n, tp)) in
        fun_param_const <|> fun_param_out <|> fun_param_free
      in
      let* params =
        lexeme
          (between (token "(") (token ")") (sep_by fun_param (many1 (token ";")))
          <|> return [])
        => List.concat
      in
      if proc
      then return (VTFunction (params, VTVoid))
      else
        let* res = token ":" >> vtype in
        return (VTFunction (params, res))
    in
    helper s
  and vtype s =
    let string_arg =
      option VTNDString (between (token "[") (token "]") expr => fun e -> VTString e)
    in
    let arr_arg =
      let interval =
        let* start = expr in
        let* fin = token ".." >> expr in
        return (start, fin)
      in
      let* intervals = between (token "[") (token "]") (sep_by1 interval (token ",")) in
      let* arr_type = token "of" >> vtype in
      return (List.fold_right (fun (s, f) t -> VTArray (s, f, t)) intervals arr_type)
    in
    let record_arg =
      sep_by as_var (many1 (token ";"))
      << many (token ";")
      << token "end"
      => List.concat
      >>= compress_DType_list (fun n tp -> n, tp)
      => fun lst -> VTRecord lst
    in
    let simple =
      word
      >>= function
      | "boolean" -> return VTBool
      | "integer" -> return VTInt
      | "real" -> return VTFloat
      | "char" -> return VTChar
      | "string" -> string_arg
      | "array" -> arr_arg
      | "record" -> record_arg
      | "function" -> function_arg false
      | "procedure" -> function_arg true
      | n -> return (VTCustom n)
    in
    simple s
  and as_var s =
    let helper =
      let* vars = names in
      let* tp = token ":" >> vtype in
      let* exp = option None (token "=" >> expr => fun exp -> Some exp) in
      let helper n acc =
        match exp with
        | None -> DNDVariable (n, tp) :: acc
        | Some exp -> DVariable (n, tp, exp) :: acc
      in
      return (List.fold_right helper vars [])
    in
    helper s
  in
  let as_type =
    let* vars = names in
    let* tp = token "=" >> vtype in
    return (List.fold_right (fun n acc -> DType (n, tp) :: acc) vars [])
  in
  let as_const =
    let* var = name in
    let* exp = token "=" >> expr in
    return (DConst (var, exp))
  in
  let as_function =
    let* ptoc = token "function" &> false <|> (token "procedure" &> true) in
    let* func_name = name in
    let* func_type = function_arg ptoc << token ";" in
    let* funct_prog = program in
    match func_type with
    | VTFunction (params, result_type) ->
      return (DFunction (func_name, result_type, params, funct_prog))
    | _ -> mzero
  in
  let block =
    key_word
    >>= function
    | "type" -> end_by as_type (token ";") => List.concat
    | "const" -> end_by as_const (token ";")
    | "var" ->
      let* vars = end_by as_var (token ";") => List.concat in
      let* functions = end_by as_function (token ";") in
      return (vars @ functions)
    | _ -> mzero
  in
  (many block => List.concat) s

and program s =
  let helper =
    let* def = definition in
    let* prog =
      between
        (token "begin")
        (many (token ";") >> token "end")
        (sep_by statement (token ";"))
    in
    return (def, prog)
  in
  helper s
;;

let%test "definition 1" =
  check_parser
    definition
    "var arr : array [1..10] of array ['a'..'b'] of integer;"
    [ DNDVariable
        ( "arr"
        , VTArray
            ( Const (VInt 1)
            , Const (VInt 10)
            , VTArray (Const (VChar 'a'), Const (VChar 'b'), VTInt) ) )
    ]
;;

let%test "definition 2" =
  check_parser
    definition
    "var a, b: integer;"
    [ DNDVariable ("a", VTInt); DNDVariable ("b", VTInt) ]
;;

let%test "definition 3" =
  check_parser
    definition
    "type add = function (x : integer; out y : integer) : integer;"
    [ DType ("add", VTFunction ([ FPFree ("x", VTInt); FPOut ("y", VTInt) ], VTInt)) ]
;;

let%test "definition 4" =
  check_parser
    definition
    "var p : procedure (const x : string);"
    [ DNDVariable ("p", VTFunction ([ FPConst ("x", VTNDString) ], VTVoid)) ]
;;

let%test "definition 5" =
  check_parser
    definition
    "var i : integer = 42;"
    [ DVariable ("i", VTInt, Const (VInt 42)) ]
;;

let%test "definition 6" =
  check_parser
    definition
    "var r : record i : integer; f : real; end;"
    [ DNDVariable ("r", VTRecord [ "i", VTInt; "f", VTFloat ]) ]
;;

let pascal_program = program << token "." << many any

let parse s =
  match use_parser pascal_program s with
  | Some (t, _) -> Some t
  | _ -> None
;;

let%test "Hello world" =
  check_parser
    pascal_program
    "begin writeln(\'hello world\'); end."
    ([], [ ProcCall (Call (Variable "writeln", [ Const (VString "hello world") ])) ])
;;

let%test "Create and use add func" =
  check_parser
    pascal_program
    "\n\
    \      var\n\
    \        x, y : integer;\n\
    \        function add (x, y : integer) : integer;\n\
    \        begin\n\
    \          add := x + y;\n\
    \        end;\n\
    \      begin\n\
    \        readln(x);\n\
    \        readln(y);\n\
    \        writeln(\'x + y = \', add(x, y));\n\
    \      end.\n\
    \    "
    ( [ DNDVariable ("x", VTInt)
      ; DNDVariable ("y", VTInt)
      ; DFunction
          ( "add"
          , VTInt
          , [ FPFree ("x", VTInt); FPFree ("y", VTInt) ]
          , ([], [ Assign (Variable "add", BinOp (Add, Variable "x", Variable "y")) ]) )
      ]
    , [ ProcCall (Call (Variable "readln", [ Variable "x" ]))
      ; ProcCall (Call (Variable "readln", [ Variable "y" ]))
      ; ProcCall
          (Call
             ( Variable "writeln"
             , [ Const (VString "x + y = ")
               ; Call (Variable "add", [ Variable "x"; Variable "y" ])
               ] ))
      ] )
;;
