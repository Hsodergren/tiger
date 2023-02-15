%{
    [@@@ocaml.warning "-27"]
    open Ast
%}

%token TYPE VAR
%token FUNCTION
%token BREAK
%token END
%token NIL
%token LET DO IN OF TO
%token FOR WHILE
%token ELSE THEN IF
%token ARRAY
%token ASSIGN
%token OR
%token AND
%token GE GT LE LT NEQ EQ
%token DIVIDE TIMES MINUS PLUS
%token DOT
%token RBRACE LBRACE RBRACK LBRACK RPAREN LPAREN
%token SEMICOLON COLON COMMA
%token <string>STRING
%token <int>INT
%token <string>ID
%token EOF

/* lowest */
%nonassoc THEN
%nonassoc ELSE
%nonassoc ASSIGN
%left SEMICOLON
%left OR
%left AND
%nonassoc EQ NEQ GT GE LT LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc HIGHEST
/* highest */

%start <Ast.t> main

%%

main:
  | e=exp EOF {e}

exp:
  | NIL { Empty }
  | LPAREN e=expseq RPAREN { Empty }
  | v=lvalue { Empty }
  | LET decs IN expseq END { Empty }
  | LPAREN RPAREN { Empty }     /* empty value */
  | s=STRING { Empty }
  | i=INT { Empty }
  | MINUS e=exp %prec HIGHEST { Empty }                   /* negation */
  | i=ID LPAREN RPAREN { Empty }            /* function application no parameters*/
  | i=ID LPAREN ps=paramlist RPAREN {Empty} /* function application with parameters */
  | e1=exp PLUS e2=exp {Empty}
  | e1=exp MINUS e2=exp {Empty}
  | e1=exp TIMES e2=exp {Empty}
  | e1=exp DIVIDE e2=exp {Empty}
  | e1=exp EQ e2=exp {Empty}
  | e1=exp NEQ e2=exp {Empty}
  | e1=exp LT e2=exp {Empty}
  | e1=exp GT e2=exp {Empty}
  | e1=exp GE e2=exp {Empty}
  | e1=exp LE e2=exp {Empty}
  | e1=exp AND e2=exp {Empty}
  | e1=exp OR e2=exp {Empty}
  | tid=ID LBRACE RBRACE { Empty } /* empty record creation */
  | tid=ID LBRACE recordlist RBRACE { Empty } /* record creation */
  | tid=lvalue LBRACK e1=exp RBRACK OF e2=exp { Empty } /* array creation */
  | v=lvalue ASSIGN e=exp { Empty }
  | IF e1=exp THEN e2=exp ELSE e3=exp { Empty }
  | IF e1=exp THEN e2=exp { Empty }
  | WHILE e1=exp DO e2=exp { Empty }
  | FOR id=ID ASSIGN e1=exp TO e2=exp DO e3=exp { Empty }
  | BREAK { Empty }

expseq:
  | { Empty }
  | exp { Empty }
  | exp SEMICOLON expseq { Empty }

recordlist:
  | id=ID EQ e=exp { Empty }
  | id=ID EQ e=exp COMMA es=recordlist { Empty }

paramlist:
  | exp {Empty}
  | exp COMMA ps=paramlist {Empty}


/* declarations */
decs:
  | {Empty}
  | d=dec ds=decs {Empty}

dec:
  | tydec { Empty }
  | vardec { Empty }
  | fundec { Empty }

tydec:
  | TYPE name=ID EQ typ=ty {Empty}

ty:
  | id=ID {Empty}
  | LBRACE fs=tyfields RBRACE {Empty}
  | ARRAY OF id=ID {Empty}

tyfields:
  | {Empty}
  | tyfield {Empty}
  | tyfield COMMA tyfields {Empty}

tyfield:
  | id=ID COLON tid=ID {Empty}

vardec:
  | VAR id=ID ASSIGN e=exp {Empty}
  | VAR id=ID COLON tid=ID ASSIGN e=exp {Empty}

fundec:
  | FUNCTION id=ID LPAREN ps=tyfields RPAREN EQ e=exp {Empty}
  | FUNCTION id=ID LPAREN ps=tyfields RPAREN COLON tid=ID EQ e=exp {Empty}

lvalue:
  | id=ID {Empty}
  | v=lvalue DOT id=ID {Empty}
  | v=lvalue LBRACK e=exp RBRACK {Empty}
