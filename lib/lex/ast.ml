type pos = Lexing.position * Lexing.position

let pp_pos pf ((p1, p2) : pos) =
  Fmt.pf pf "[%d,%d]@ -@ [%d,%d]" p1.pos_lnum p1.pos_cnum p2.pos_lnum
    p2.pos_cnum

type var =
  | SimpleVar of Symbol.t * pos
  | FieldVar of var * Symbol.t * pos
  | SubscriptVar of var * exp * pos

and exp =
  | Empty
  | NilExp
  | VarExp of var
  | IntExp of int
  | StringExp of string
  | CallExp of { func : Symbol.t; args : exp list; pos : pos }
  | OpExp of { left : exp; oper : oper; right : exp; pos : pos }
  | RecordExp of {
      typ : Symbol.t;
      fields : (Symbol.t * exp * pos) list;
      pos : pos;
    }
  | SeqExp of (exp * pos) list
  | AssignExp of { var : var; exp : exp; pos : pos }
  | IfExp of { pred : exp; then' : exp; else' : exp option; pos : pos }
  | WhileExp of { pred : exp; body : exp; pos : pos }
  | ForExp of {
      var : Symbol.t;
      escape : bool ref;
      from' : exp;
      to' : exp;
      body : exp;
      pos : pos;
    }
  | Break of pos
  | LetExp of { decs : dec list; body : exp; pos : pos }
  | ArrayExp of { typ : Symbol.t; size : exp; init : exp; pos : pos }

and oper = Plus | Minus | Mult | Div | Gt | Ge | Lt | Le | Eq | Neq

and dec =
  | FunctionDec of fundec list
  | VarDec of vardec
  | TypeDec of typdec list

and fundec = {
  f_name : Symbol.t;
  f_params : field list;
  f_rettyp : (Symbol.t * pos) option;
  f_body : exp;
  f_pos : pos;
}

and vardec = {
  vvar : Symbol.t;
  vescape : bool ref;
  vtyp : (Symbol.t * pos) option;
  vinit : exp;
  vpos : pos;
}

and typdec = { tsym : Symbol.t; ttyp : typ; tpos : pos }
and field = { name : Symbol.t; escape : bool ref; typ : Symbol.t; pos : pos }

and typ =
  | NameTy of Symbol.t * pos
  | RecordTy of field list * pos
  | ArrayTy of Symbol.t * pos
[@@deriving show]

let rec collect_mut_decs = function
  | ([] | [ _ ]) as l -> l
  | FunctionDec f1 :: FunctionDec f2 :: tl ->
      collect_mut_decs (FunctionDec (f1 @ f2) :: tl)
  | TypeDec t1 :: TypeDec t2 :: tl -> collect_mut_decs (TypeDec (t1 @ t2) :: tl)
  | hd :: tl -> hd :: collect_mut_decs tl
