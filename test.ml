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

let test_cases () =
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
  let test_interp input expected_output =
    let result = interp input top_env in
    assert (result = expected_output)
  in

  (* Test cases for numbers, strings, and variable lookup *)
  test_interp (NumC 42.0) (NumV 42.0);
  test_interp (StringC "hello") (StringV "hello");
  test_interp (IdC "true") (BooleanV true);

  (* Test case for if expressions *)
  test_interp (IfC (IdC "true", NumC 1.0, NumC 2.0)) (NumV 1.0);
  test_interp (IfC (IdC "false", NumC 1.0, NumC 2.0)) (NumV 2.0);

  (* Test case for lambda expressions *)
  let lam = LamC (["x"; "y"], AppC (IdC "+", [IdC "x"; IdC "y"])) in
  test_interp lam (ClosV (["x"; "y"], AppC (IdC "+", [IdC "x"; IdC "y"]), top_env));

  (* Test case for function application *)
  let add = AppC (lam, [NumC 1.0; NumC 2.0]) in
  test_interp add (NumV 3.0);

  (* Test case for nested expressions *)
  let nested = IfC (IdC "true", AppC (lam, [NumC 10.0; NumC 20.0]), NumC 0.0) in
  test_interp nested (NumV 30.0);

  (* Add more test cases as needed *)
  ()

(* Call the test_cases function *)
test_cases ()
