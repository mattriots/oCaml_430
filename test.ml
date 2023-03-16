type exprc =
  | NumC of float
  | StringC of string
  | IdC of string
  | LamC of string list * exprc
  | AppC of exprc * exprc list
  | IfC of exprc * exprc * exprc

type value =
  | NumV of float
  | BooleanV of bool
  | StringV of string
  | ClosV of string list * exprc * env
  | PrimV of string

and binding = Binding of string * value

and env = binding list

let rec lookup env x =
  match env with
  | [] -> failwith ("Unbound variable: " ^ x)
  | Binding (y, v) :: rest -> if x = y then v else lookup rest x

let rec interp expr env =
  match expr with
  | NumC n -> NumV n
  | StringC s -> StringV s
  | IdC x -> lookup env x
  | LamC (args, body) -> ClosV (args, body, env)
  | AppC (fun_expr, arg_exprs) ->
      let fun_value = interp fun_expr env in
      let arg_values = List.map (fun arg -> interp arg env) arg_exprs in
      apply fun_value arg_values
  | IfC (test_expr, then_expr, else_expr) ->
      match interp test_expr env with
      | BooleanV true -> interp then_expr env
      | BooleanV false -> interp else_expr env
      | _ -> failwith "Condition must be a boolean value"

and apply func args =
  match func with
  | PrimV prim_name -> apply_primitive prim_name args
  | ClosV (params, body, closure_env) ->
      if List.length params <> List.length args
      then failwith "Incorrect number of arguments"
      else
        let new_bindings = List.map2 (fun param arg -> Binding (param, arg)) params args in
        let extended_env = new_bindings @ closure_env in
        interp body extended_env
  | _ -> failwith "Application of non-function value"

and apply_primitive prim_name args =
  match (prim_name, args) with
  | ("+", [NumV x; NumV y]) -> NumV (x +. y)
  | ("-", [NumV x; NumV y]) -> NumV (x -. y)
  | ("*", [NumV x; NumV y]) -> NumV (x *. y)
  | ("/", [NumV x; NumV y]) -> NumV (x /. y)
  | ("<=", [NumV x; NumV y]) -> BooleanV (x <= y)
  | ("equal?", [x; y]) -> BooleanV (x = y)
  | ("error", _) -> failwith "Error primitive called"
  | _ -> failwith "Invalid primitive or incorrect number of arguments"

  let simple_test () =
    let top_env = [
      Binding ("+", PrimV "+");
      Binding ("-", PrimV "-");
      Binding ("*", PrimV "*");
      Binding ("/", PrimV "/");
      Binding ("<=", PrimV "<=");
      Binding ("equal?", PrimV "equal?");
      Binding ("true", BooleanV true);
      Binding ("false", BooleanV false);
      Binding ("error", PrimV "error");
    ] in
    let result = interp (NumC 42.0) top_env in
    assert (result = NumV 42.0)
  ;;
  
simple_test ();;

