open Ast

type env = {function_table : (string, function_definition) Hashtbl.t}

let build_env funcs = let tbl = Hashtbl.create 10 in
  List.iter (fun func -> Hashtbl.add tbl func.name func) funcs;
  {function_table = tbl}

let env_lookup env name = Hashtbl.find env.function_table name

let rec comp env func args = 
  let vars = Hashtbl.create 10 in
  (List.iter2 (fun name arg -> Hashtbl.add vars name arg) func.params args;
   match eval_stmt env vars func.body with Some x -> x | None -> 0)

and eval_stmt env vars = function
  Expr expr -> ignore (eval_expr env vars expr); None
| If (expr, t, e) ->
    if (eval_expr env vars expr) != 0 then eval_stmt env vars t else eval_stmt env vars e
| While (expr, stmt) ->
    let res = ref None in
    while (eval_expr env vars expr) != 0 && (res := eval_stmt env vars stmt; !res = None) do () done;
    !res
| Block (definedVars, stmts) ->
    List.iter (fun (name, expr) -> Hashtbl.add vars name (eval_expr env vars expr)) definedVars;
    eval_stmt_list env vars stmts
| Return expr -> Some (eval_expr env vars expr)

and eval_stmt_list env vars = function
  [] -> None
| stmt :: rest -> (match eval_stmt env vars stmt with
      Some x -> Some x
    | None -> eval_stmt_list env vars rest)

and eval_expr env vars = function
  Constant c -> c
| Identifier name -> Hashtbl.find vars name
| Call ("print", args) -> 
    print_int (eval_expr env vars (List.hd args)); print_newline (); 0
| Call (name, args) ->
    let func = Hashtbl.find env.function_table name in
    comp env func (List.map (eval_expr env vars) args)   
| UnalyPlus expr -> eval_expr env vars expr
| UnalyMinus expr -> -(eval_expr env vars expr)
| Mul (lhs, rhs) -> (eval_expr env vars lhs) * (eval_expr env vars rhs)
| Div (lhs, rhs) -> (eval_expr env vars lhs) / (eval_expr env vars rhs)
| Mod (lhs, rhs) -> (eval_expr env vars lhs) mod (eval_expr env vars rhs)
| Add (lhs, rhs) -> (eval_expr env vars lhs) + (eval_expr env vars rhs)
| Sub (lhs, rhs) -> (eval_expr env vars lhs) - (eval_expr env vars rhs)
| Lt (lhs, rhs) -> if (eval_expr env vars lhs) < (eval_expr env vars rhs) then 1 else 0
| Gt (lhs, rhs) -> if (eval_expr env vars lhs) > (eval_expr env vars rhs) then 1 else 0
| Lteq (lhs, rhs) -> if (eval_expr env vars lhs) <= (eval_expr env vars rhs) then 1 else 0
| Gteq (lhs, rhs) -> if (eval_expr env vars lhs) >= (eval_expr env vars rhs) then 1 else 0
| Eq (lhs, rhs) -> if (eval_expr env vars lhs) = (eval_expr env vars rhs) then 1 else 0
| Ne (lhs, rhs) -> if (eval_expr env vars lhs) != (eval_expr env vars rhs) then 1 else 0
| Assign (name, expr) -> (let v = (eval_expr env vars expr) in
    Hashtbl.add vars name v; v)
