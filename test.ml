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
    ]

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


  let numC_test () =
  let result = interp (NumC 42.0) top_env in
  assert (result = NumV 42.0) 
  ;;

  let idC_test () =
  let local_env = [
    Binding ("x", NumV 42.0);
    Binding ("y", StringV "hello");
    Binding ("z", BooleanV true);
  ] 
  in 
  let local_top_env = local_env @ top_env in
  let expr1 = IdC "x" in
  let result1 = interp expr1 local_top_env in
  assert (result1 = NumV 42.0);

  let expr2 = IdC "y" in
  let result2 = interp expr2 local_top_env in
  assert (result2 = StringV "hello");

  let expr3 = IdC "z" in
  let result3 = interp expr3 local_top_env in
  assert (result3 = BooleanV true);;

  let stringC_test () =
  let result = interp (StringC "hello world") top_env in
  assert (result = StringV "hello world") ;;

  let lamC_test () =
  let result = interp (LamC (["x"], AppC (IdC "+", [IdC "x"; NumC 2.0]))) top_env in
  match result with
  | ClosV ([arg], body, env) ->
      let arg_value = NumV 3.0 in
      let new_env = Binding (arg, arg_value) :: env in
      let final_result = interp body new_env in
      assert (final_result = NumV 5.0)
  | _ -> failwith "Expected a closure value" ;;

let appC_test () =
  let expr = AppC (IdC "+", [NumC 2.0; NumC 3.0]) in
  let result = interp expr top_env in
  assert (result = NumV 5.0) ;;


let ifC_test () =
  let expr = IfC (IdC "true", NumC 42.0, NumC 100.0) in
  let result = interp expr top_env in
  assert (result = NumV 42.0);

  let expr2 = IfC (IdC "false", NumC 42.0, NumC 100.0) in
  let result2 = interp expr2 top_env in
  assert (result2 = NumV 100.0);

  let expr3 = IfC (StringC "non-boolean value", NumC 42.0, NumC 100.0) in
  assert (try interp expr3 top_env |> ignore; false with _ -> true) ;;

let env_test () =
  let expr = AppC (
    LamC (["x"; "y"],
      AppC (IdC "+",
        [IdC "x";
         AppC (LamC (["x"],
                 AppC (IdC "*",
                   [IdC "x";
                    NumC 2.0])),
               [IdC "y"])])),
    [NumC 3.0; NumC 4.0]) in
    let result = interp expr top_env in
    assert (result = NumV 11.0);;

numC_test ();;
stringC_test ();;
idC_test ();;
lamC_test ();;
appC_test();;
ifC_test();;
