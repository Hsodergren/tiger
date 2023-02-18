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
%left OR
%left AND
%nonassoc EQ NEQ GT GE LT LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc HIGHEST
/* highest */

%start <Ast.exp> main

%%

main:
  | e=exp EOF {e}

exp:
  | NIL { Ast.NilExp }
  | LPAREN e=expseq RPAREN { Ast.SeqExp e }
  | v=lvalue { VarExp v }
  | LET d=decs IN e=expseq END { LetExp {decs=Ast.collect_mut_decs d; body=SeqExp e; pos=$loc} }
  | s=STRING { StringExp s }
  | i=INT { IntExp i }
  | MINUS e=exp %prec HIGHEST { OpExp {left=IntExp 0; oper=Ast.Minus; right=e; pos=$loc} } /* negation */
  | i=ID LPAREN RPAREN { CallExp {func=Symbol.v i; args=[]; pos=$loc} } /* function application no parameters*/
  | i=ID LPAREN ps=paramlist RPAREN {CallExp {func=Symbol.v i; args=ps; pos=$loc}} /* function application with parameters */
  | e1=exp PLUS e2=exp { OpExp {left=e1; oper=Ast.Plus; right=e2; pos=$loc} }
  | e1=exp MINUS e2=exp { OpExp {left=e1; oper=Ast.Minus; right=e2; pos=$loc} }
  | e1=exp TIMES e2=exp { OpExp {left=e1; oper=Ast.Mult; right=e2; pos=$loc} }
  | e1=exp DIVIDE e2=exp { OpExp {left=e1; oper=Ast.Div; right=e2; pos=$loc} }
  | e1=exp EQ e2=exp { OpExp {left=e1; oper=Ast.Eq; right=e2; pos=$loc} }
  | e1=exp NEQ e2=exp { OpExp {left=e1; oper=Ast.Neq; right=e2; pos=$loc} }
  | e1=exp LT e2=exp { OpExp {left=e1; oper=Ast.Lt; right=e2; pos=$loc} }
  | e1=exp LE e2=exp { OpExp {left=e1; oper=Ast.Le; right=e2; pos=$loc} }
  | e1=exp GT e2=exp { OpExp {left=e1; oper=Ast.Gt; right=e2; pos=$loc} }
  | e1=exp GE e2=exp { OpExp {left=e1; oper=Ast.Ge; right=e2; pos=$loc} }
  | e1=exp AND e2=exp { IfExp {pred=e1; then'=e2; else'=Some (IntExp 0); pos=$loc} }
  | e1=exp OR e2=exp { IfExp {pred=e1; then'=IntExp 1; else'=Some e2; pos=$loc} }
  | tid=ID LBRACE RBRACE { RecordExp {typ=Symbol.v tid; fields=[]; pos=$loc } } /* empty record creation */
  | tid=ID LBRACE fields=recordlist RBRACE { RecordExp { typ=Symbol.v tid; fields; pos=$loc} } /* record creation */
  | tid=lvalue LBRACK e1=exp RBRACK OF e2=exp
    {
      match tid with
      | SimpleVar (sym,_) -> ArrayExp {typ=sym; size=e1; init=e2; pos=$loc}
      | _ -> failwith "error"
    } /* array creation */
  | v=lvalue ASSIGN e=exp { AssignExp {var=v; exp=e; pos=$loc} }
  | IF e1=exp THEN e2=exp ELSE e3=exp { IfExp { pred=e1; then'=e2; else'=Some e3; pos=$loc} }
  | IF e1=exp THEN e2=exp { IfExp {pred=e1; then'=e2; else'=None; pos=$loc } }
  | WHILE e1=exp DO e2=exp { WhileExp { pred=e1; body=e2; pos=$loc} }
  | FOR id=ID ASSIGN e1=exp TO e2=exp DO e3=exp
    {
      ForExp {var=Symbol.v id;
              escape=ref false;
              from'=e1;
              to'=e2;
              body=e3;
              pos=$loc
             }
    }
  | BREAK { Break $loc }

expseq:
  | { [] }
  | e=exp { [e,$loc] }
  | e=exp SEMICOLON tl=expseq { (e,$loc)::tl }

recordlist:
  | id=ID EQ e=exp { [Symbol.v id, e, $loc] }
  | id=ID EQ e=exp COMMA es=recordlist { (Symbol.v id, e, $loc)::es }

paramlist:
  | e=exp {[e]}
  | e=exp COMMA ps=paramlist {e::ps}

/* declarations */
decs:
  | {[]}
  | d=dec ds=decs { d::ds }

dec:
  | t=tydec { TypeDec t }
  | v=vardec { VarDec v }
  | f=fundec { FunctionDec f }

tydec:
  | TYPE name=ID EQ typ=ty { [{tsym=Symbol.v name; ttyp=typ; tpos=$loc}] }

ty:
  | id=ID { NameTy (Symbol.v id, $loc) }
  | LBRACE fs=tyfields RBRACE { RecordTy (fs,$loc) }
  | ARRAY OF id=ID { ArrayTy (Symbol.v id, $loc) }

tyfields:
  | { [] }
  | t=tyfield { [t] }
  | t=tyfield COMMA ts=tyfields { t::ts }

tyfield:
  | id=ID COLON tid=ID { {name=Symbol.v id; escape=ref false; typ=Symbol.v tid; pos=$loc} }

vardec:
  | VAR id=ID ASSIGN e=exp { { vvar=Symbol.v id; vescape=ref false; vtyp=None; vinit=e; vpos=$loc } }
  | VAR id=ID COLON tid=ID ASSIGN e=exp
    {
      { vvar=Symbol.v id; vescape=ref false; vtyp=Some (Symbol.v tid, $loc); vinit=e; vpos=$loc }
    }

fundec:
  | FUNCTION id=ID LPAREN ps=tyfields RPAREN EQ e=exp
    {
      [{
        f_name=Symbol.v id;
        f_params=ps;
        f_rettyp=None;
        f_body=e;
        f_pos=$loc
      }]
    }
  | FUNCTION id=ID LPAREN ps=tyfields RPAREN COLON tid=ID EQ e=exp
    {
      [{
        f_name=Symbol.v id;
        f_params=ps;
        f_rettyp=Some (Symbol.v tid, $loc);
        f_body=e;
        f_pos=$loc
      }]
    }


lvalue:
  | id=ID {Ast.SimpleVar (Symbol.v id, $loc)}
  | v=lvalue DOT id=ID { Ast.FieldVar (v, Symbol.v id, $loc)}
  | v=lvalue LBRACK e=exp RBRACK { Ast.SubscriptVar (v, e, $loc) }
