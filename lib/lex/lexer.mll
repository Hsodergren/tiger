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
  | "while" {Parser.WHILE}
  | "for" {Parser.FOR}
  | "to" {Parser.TO}
  | "break" {Parser.BREAK}
  | "let" {Parser.LET}
  | "in" {Parser.IN}
  | "end" {Parser.END}
  | "function" {Parser.FUNCTION}
  | "var" {Parser.VAR}
  | "type" {Parser.TYPE}
  | "array" {Parser.ARRAY}
  | "if" {Parser.IF}
  | "then" {Parser.THEN}
  | "else" {Parser.ELSE}
  | "do" {Parser.DO}
  | "of" {Parser.OF}
  | "nil" {Parser.NIL}
  | ":=" {Parser.ASSIGN}
  | "," {Parser.COMMA}
  | ":" {Parser.COLON}
  | ";" {Parser.SEMICOLON}
  | "(" {Parser.LPAREN}
  | ")" {Parser.RPAREN}
  | "[" {Parser.LBRACK}
  | "]" {Parser.RBRACK}
  | "{" {Parser.LBRACE}
  | "}" {Parser.RBRACE}
  | "." {Parser.DOT}
  | "+" {Parser.PLUS}
  | "-" {Parser.MINUS}
  | "*" {Parser.TIMES}
  | "/" {Parser.DIVIDE}
  | "=" {Parser.EQ}
  | "<>" {Parser.NEQ}
  | "<" {Parser.LT}
  | "<=" {Parser.LE}
  | ">" {Parser.GT}
  | ">=" {Parser.GE}
  | "&" {Parser.AND}
  | "|" {Parser.OR}
  | "/*" {comment 0 lexbuf}
  | ['a'-'z' 'A'-'Z'] (alphanumeric|"_")* as id {Parser.ID id}
  | number+ as i {Parser.INT (int_of_string i)}
  | '"' {Parser.STRING (string (Buffer.create 0) lexbuf)}
  | '\n' {nextLine (1 + lpos lexbuf); tokenize lexbuf}
  | whitespace {tokenize lexbuf}
  | eof {Parser.EOF}
and comment level = parse
  | "/*" {comment (level + 1) lexbuf}
  | "*/" {if level = 0 then tokenize lexbuf else comment (level-1) lexbuf}
  | '\n' {nextLine (1 + lpos lexbuf); comment level lexbuf}
  | eof {failwith "Unclosed comment at EOF"}
  | _ {comment level lexbuf}
and string buf = parse
  | '"' {
      let result = Buffer.contents buf in
      result
    }
  | _ as c {Buffer.add_char buf c; string buf lexbuf}
  | "\\n" {Buffer.add_char buf '\n'; string buf lexbuf}
  | "\\t" {Buffer.add_char buf '\t'; string buf lexbuf}
  | "\\r" {Buffer.add_char buf '\r'; string buf lexbuf}
  | "\\\"" {Buffer.add_char buf '"'; string buf lexbuf}
