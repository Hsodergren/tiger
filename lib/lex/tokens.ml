type linenum = int
type token = string

let _TYPE (i, _) = "TYPE   " ^ string_of_int i
let _VAR (i, _) = "VAR   " ^ string_of_int i
let _FUNCTION (i, _) = "FUNCTION   " ^ string_of_int i
let _BREAK (i, _) = "BREAK   " ^ string_of_int i
let _OF (i, _) = "OF   " ^ string_of_int i
let _END (i, _) = "END   " ^ string_of_int i
let _IN (i, _) = "IN   " ^ string_of_int i
let _NIL (i, _) = "NIL   " ^ string_of_int i
let _LET (i, _) = "LET   " ^ string_of_int i
let _DO (i, _) = "DO   " ^ string_of_int i
let _TO (i, _) = "TO   " ^ string_of_int i
let _FOR (i, _) = "FOR   " ^ string_of_int i
let _WHILE (i, _) = "WHILE   " ^ string_of_int i
let _ELSE (i, _) = "ELSE   " ^ string_of_int i
let _THEN (i, _) = "THEN   " ^ string_of_int i
let _IF (i, _) = "IF   " ^ string_of_int i
let _ARRAY (i, _) = "ARRAY   " ^ string_of_int i
let _ASSIGN (i, _) = "ASSIGN   " ^ string_of_int i
let _OR (i, _) = "OR   " ^ string_of_int i
let _AND (i, _) = "AND   " ^ string_of_int i
let _GE (i, _) = "GE   " ^ string_of_int i
let _GT (i, _) = "GT   " ^ string_of_int i
let _LE (i, _) = "LE   " ^ string_of_int i
let _LT (i, _) = "LT   " ^ string_of_int i
let _NEQ (i, _) = "NEQ   " ^ string_of_int i
let _EQ (i, _) = "EQ   " ^ string_of_int i
let _DIVIDE (i, _) = "DIVIDE   " ^ string_of_int i
let _TIMES (i, _) = "TIMES   " ^ string_of_int i
let _MINUS (i, _) = "MINUS   " ^ string_of_int i
let _PLUS (i, _) = "PLUS   " ^ string_of_int i
let _DOT (i, _) = "DOT   " ^ string_of_int i
let _RBRACE (i, _) = "RBRACE   " ^ string_of_int i
let _LBRACE (i, _) = "LBRACE   " ^ string_of_int i
let _RBRACK (i, _) = "RBRACK   " ^ string_of_int i
let _LBRACK (i, _) = "LBRACK   " ^ string_of_int i
let _RPAREN (i, _) = "RPAREN   " ^ string_of_int i
let _LPAREN (i, _) = "LPAREN   " ^ string_of_int i
let _SEMICOLON (i, _) = "SEMICOLON   " ^ string_of_int i
let _COLON (i, _) = "COLON   " ^ string_of_int i
let _COMMA (i, _) = "COMMA   " ^ string_of_int i
let _STRING (s, i, _) = "STRING(" ^ s ^ ")     " ^ string_of_int i
let _INT (c, i, _) = "INT(" ^ string_of_int c ^ ")   " ^ string_of_int i
let _ID (s, i, _) = "ID(" ^ s ^ ")     " ^ string_of_int i
let _EOF (i, _) = "EOF   " ^ string_of_int i
