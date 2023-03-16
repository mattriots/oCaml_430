
(* (define-type ExprX (U numX idX stringX if appX lamX))
(struct numX ([n : Real])#:transparent)
(struct idX ([s : Symbol])#:transparent)
(struct stringX ([s : String])#:transparent)
(struct if ([test : ExprX] [then : ExprX] [else : ExprX])#:transparent)
(struct appX ([body : ExprX] [arg : (Listof ExprX)])#:transparent)
(struct lamX ([args : (Listof Symbol)] [body : ExprX]) #:transparent)*)


type exprX = 
            | NumX of int
            | IdX of string
            | BoolX of bool
            | StringX of string
            | IfX of exprX * exprX * exprX
            | AppX of exprX * exprX list
            | LamX of string list * exprX ;;

type value = 
            | NumV of int
            | BoolV of bool
            | StringV of string
            | ClosureV of string list * exprX * (string * value)
            | PrimV of string ;;

let string_of_value v =
  match v with
  | NumV v -> string_of_int v
  | StringV s -> s
  | BoolV b -> string_of_bool b
  | ClosureV _ -> "<function>"
  | PrimV op -> op
            

let rec interp e env =
  match e with
  | NumX e -> NumV e
  | IdX e -> List.assoc e env
  | StringX e -> StringV e 
  | IfX (test, then_, else_) ->
     let test = interp test env in
     begin match test with
     | BoolV true -> interp then_ env
     | BoolV false -> interp else_ env
     | _ -> failwith "if: non-boolean test"
     end ;;

let test = interp (NumX 3) [] in
Printf.printf "%s\n" (string_of_value test) ;;

let test_string = interp (StringX "hello world") [] in
Printf.printf "%s\n" (string_of_value test_string) ;;

let test_if = interp (IfX ((BoolX true), (NumX 1), (NumX 2)))  [] in
Printf.printf "%s\n" (string_of_value test_if) ;;