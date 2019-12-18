{
let lineNum = Errormsg.ErrorMsg.lineNum
let linePos = Errormsg.ErrorMsg.linePos
let err p1 msg = Errormsg.ErrorMsg.error p1 msg
let lpos lexbuf = Lexing.lexeme_start lexbuf
let rpos lexbuf = Lexing.lexeme_end lexbuf
let pos_pair lexbuf = (lpos lexbuf, rpos lexbuf)
let nextLine pos = (lineNum := !lineNum +1; linePos := pos :: !linePos)
}

let whitespace = " " | "\t"
let number = ['0'-'9']
let alphanumeric = ['a'-'z' 'A'-'Z' '0'-'9']

rule tokenize = parse
  | "while" {print_endline (Tokens._WHILE (pos_pair lexbuf)); tokenize lexbuf}
  | "for" {print_endline (Tokens._FOR (pos_pair lexbuf)); tokenize lexbuf}
  | "to" {print_endline (Tokens._TO (pos_pair lexbuf)); tokenize lexbuf}
  | "break" {print_endline (Tokens._BREAK (pos_pair lexbuf)); tokenize lexbuf}
  | "let" {print_endline (Tokens._LET (pos_pair lexbuf)); tokenize lexbuf}
  | "in" {print_endline (Tokens._IN (pos_pair lexbuf)); tokenize lexbuf}
  | "end" {print_endline (Tokens._END (pos_pair lexbuf)); tokenize lexbuf}
  | "function" {print_endline (Tokens._FUNCTION (pos_pair lexbuf)); tokenize lexbuf}
  | "var" {print_endline (Tokens._VAR (pos_pair lexbuf)); tokenize lexbuf}
  | "type" {print_endline (Tokens._TYPE (pos_pair lexbuf)); tokenize lexbuf}
  | "array" {print_endline (Tokens._ARRAY (pos_pair lexbuf)); tokenize lexbuf}
  | "if" {print_endline (Tokens._IF (pos_pair lexbuf)); tokenize lexbuf}
  | "then" {print_endline (Tokens._THEN (pos_pair lexbuf)); tokenize lexbuf}
  | "else" {print_endline (Tokens._ELSE (pos_pair lexbuf)); tokenize lexbuf}
  | "do" {print_endline (Tokens._DO (pos_pair lexbuf)); tokenize lexbuf}
  | "of" {print_endline (Tokens._OF (pos_pair lexbuf)); tokenize lexbuf}
  | "nil" {print_endline (Tokens._NIL (pos_pair lexbuf)); tokenize lexbuf}
  | "," {print_endline (Tokens._COMMA (pos_pair lexbuf)); tokenize lexbuf}
  | ":" {print_endline (Tokens._COLON (pos_pair lexbuf)); tokenize lexbuf}
  | ";" {print_endline (Tokens._SEMICOLON (pos_pair lexbuf)); tokenize lexbuf}
  | "(" {print_endline (Tokens._LPAREN (pos_pair lexbuf)); tokenize lexbuf}
  | ")" {print_endline (Tokens._RPAREN (pos_pair lexbuf)); tokenize lexbuf}
  | "[" {print_endline (Tokens._RBRACK (pos_pair lexbuf)); tokenize lexbuf}
  | "]" {print_endline (Tokens._LBRACK (pos_pair lexbuf)); tokenize lexbuf}
  | "{" {print_endline (Tokens._RBRACE (pos_pair lexbuf)); tokenize lexbuf}
  | "}" {print_endline (Tokens._LBRACE (pos_pair lexbuf)); tokenize lexbuf}
  | "." {print_endline (Tokens._DOT (pos_pair lexbuf)); tokenize lexbuf}
  | "+" {print_endline (Tokens._PLUS (pos_pair lexbuf)); tokenize lexbuf}
  | "-" {print_endline (Tokens._MINUS (pos_pair lexbuf)); tokenize lexbuf}
  | "*" {print_endline (Tokens._TIMES (pos_pair lexbuf)); tokenize lexbuf}
  | "/" {print_endline (Tokens._DIVIDE (pos_pair lexbuf)); tokenize lexbuf}
  | "=" {print_endline (Tokens._EQ (pos_pair lexbuf)); tokenize lexbuf}
  | "<>" {print_endline (Tokens._NEQ (pos_pair lexbuf)); tokenize lexbuf}
  | "<" {print_endline (Tokens._LT (pos_pair lexbuf)); tokenize lexbuf}
  | "<=" {print_endline (Tokens._LE (pos_pair lexbuf)); tokenize lexbuf}
  | ">" {print_endline (Tokens._GT (pos_pair lexbuf)); tokenize lexbuf}
  | ">=" {print_endline (Tokens._GE (pos_pair lexbuf)); tokenize lexbuf}
  | "&" {print_endline (Tokens._AND (pos_pair lexbuf)); tokenize lexbuf}
  | "|" {print_endline (Tokens._OR (pos_pair lexbuf)); tokenize lexbuf}
  | "/*" {comment 0 lexbuf}
  | ['a'-'z' 'A'-'Z'] (alphanumeric|"_")* as id {print_endline (Tokens._ID (id, lpos lexbuf, rpos lexbuf)); tokenize lexbuf}
  | '"' {string (Buffer.create 0) lexbuf}
  | '\n' {nextLine (1 + lpos lexbuf); tokenize lexbuf}
  | whitespace {print_endline "whitespace"; tokenize lexbuf}
  | eof {print_endline (Tokens._EOF (pos_pair lexbuf)); exit 0}
  | _ as c  {Printf.printf "ERROR: '%c' (%d)\n" c (lpos lexbuf) ; tokenize lexbuf}
and comment level = parse
  | "/*" {comment (level + 1) lexbuf}
  | "*/" {if level = 0 then tokenize lexbuf else comment (level-1) lexbuf}
  | '\n' {nextLine (1 + lpos lexbuf); comment level lexbuf}
  | eof {err (lpos lexbuf) "Unclosed comment at EOF"; exit 1}
  | _ {comment level lexbuf}
and string buf = parse
  | '"' {
      let result = Buffer.contents buf in
      print_endline (Tokens._STRING (result, (lpos lexbuf) - (String.length result), lpos lexbuf));
      tokenize lexbuf
    }
  | "\\n" {Buffer.add_char buf '\n'; string buf lexbuf}
  | "\\t" {Buffer.add_char buf '\t'; string buf lexbuf}
  | "\\r" {Buffer.add_char buf '\r'; string buf lexbuf}
  | "\\\"" {Buffer.add_char buf '"'; string buf lexbuf}
