open Printf

type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm of exp list
and exp = IdExp of id
        | NumExp of int
        | OpExp of exp * binop * exp
        | EseqExp of stm * exp

let prog =
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a"; OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"; IdExp "b"]))


let rec listMax acc = function
  | [] -> acc
  | hd::tl -> listMax (max acc hd) tl

let maxargs stmt =
  let rec maxargsStmt = function
    | CompoundStm (stmt1, stmt2) -> max (maxargsStmt stmt1) (maxargsStmt stmt2)
    | AssignStm (_, e) -> maxargsExp e
    | PrintStm l -> max (List.length l) (listMax 0 (List.map maxargsExp l))
  and maxargsExp = function
    | IdExp _ | NumExp _-> 0
    | OpExp (e1, _, e2) -> max (maxargsExp e1) (maxargsExp e2)
    | EseqExp (s, e) -> max (maxargsStmt s) (maxargsExp e)
  in
  maxargsStmt stmt


let rec lookup id l =
  match l with
  | [] -> failwith (sprintf "Couldn't find variable %s" id)
  | (i,value)::tl -> if i = id then value else lookup id tl

let rec printList = function
  | [] -> ()
  | hd::tl -> printf "%d\n" hd; printList tl

let rec interpStmt vars = function
  | CompoundStm (s1, s2) -> interpStmt (interpStmt vars s1) s2
  | AssignStm (id, exp) -> (id, interpExp vars exp)::vars
  | PrintStm l -> printList (List.map (interpExp vars) l); vars
and interpExp vars = function
  | IdExp id -> lookup id vars
  | NumExp i -> i
  | OpExp (e1, op, e2) ->
    begin
      let v1 = interpExp vars e1 in
      let v2 = interpExp vars e2 in
        match op with
      | Plus -> (v1 + v2)
      | Minus -> (v1 - v2)
      | Times -> (v1 * v2)
      | Div -> (v1 / v2)
    end
  | EseqExp (s, e) -> let vars = interpStmt vars s in interpExp vars e



let interp stmt =
  ignore(interpStmt [] stmt)
