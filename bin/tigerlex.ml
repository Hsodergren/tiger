type tok = Lex.Parser.token =
  | WHILE
  | VAR
  | TYPE
  | TO
  | TIMES
  | THEN
  | STRING of string
  | SEMICOLON
  | RPAREN
  | RBRACK
  | RBRACE
  | PLUS
  | OR
  | OF
  | NIL
  | NEQ
  | MINUS
  | LT
  | LPAREN
  | LET
  | LE
  | LBRACK
  | LBRACE
  | INT of int
  | IN
  | IF
  | ID of string
  | GT
  | GE
  | FUNCTION
  | FOR
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | DO
  | DIVIDE
  | COMMA
  | COLON
  | BREAK
  | ASSIGN
  | ARRAY
  | AND

let _print : tok -> string =
 fun _tok ->
  match _tok with
  | AND -> "AND"
  | ARRAY -> "ARRAY"
  | ASSIGN -> "ASSIGN"
  | BREAK -> "BREAK"
  | COLON -> "COLON"
  | COMMA -> "COMMA"
  | DIVIDE -> "DIVIDE"
  | DO -> "DO"
  | DOT -> "DOT"
  | ELSE -> "ELSE"
  | END -> "END"
  | EOF -> "EOF"
  | EQ -> "EQ"
  | FOR -> "FOR"
  | FUNCTION -> "FUNCTION"
  | GE -> "GE"
  | GT -> "GT"
  | ID s -> Printf.sprintf "ID(%s)" s
  | IF -> "IF"
  | IN -> "IN"
  | INT i -> Printf.sprintf "INT(%d)" i
  | LBRACE -> "LBRACE"
  | LBRACK -> "LBRACK"
  | LE -> "LE"
  | LET -> "LET"
  | LPAREN -> "LPAREN"
  | LT -> "LT"
  | MINUS -> "MINUS"
  | NEQ -> "NEQ"
  | NIL -> "NIL"
  | OF -> "OF"
  | OR -> "OR"
  | PLUS -> "PLUS"
  | RBRACE -> "RBRACE"
  | RBRACK -> "RBRACK"
  | RPAREN -> "RPAREN"
  | SEMICOLON -> "SEMICOLON"
  | STRING s -> Printf.sprintf "STRING(%s)" s
  | THEN -> "THEN"
  | TIMES -> "TIMES"
  | TO -> "TO"
  | TYPE -> "TYPE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"

let parse filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let ast =
    Lex.Parser.main
      (fun buf ->
        let t = Lex.Lexer.tokenize buf in
        (* Fmt.pr "%s@ " (_print t); *)
        t)
      lexbuf
  in
  close_in file;
  ast

let () =
  let test_path = "/home/henrik/programming/tiger/testcases/" in
  let arr = Sys.readdir test_path in
  Array.sort String.compare arr;
  Fmt.pr "%a@." (Fmt.array Fmt.Dump.string) arr;
  Array.iter
    (fun file ->
      try
        let full_path = test_path ^ "/" ^ file in
        Fmt.pr "%s@." full_path;
        let ast = parse full_path in
        Fmt.pr "%a@.@." Lex.Ast.pp_exp ast
      with e -> Fmt.pr "%a@." Fmt.exn e)
    arr
