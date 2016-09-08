open Ast
open Big_int

type env = {function_table : (string, function_definition) Hashtbl.t}

let ( +~ ) = add_big_int;;
let ( -~ ) = sub_big_int;;
let ( *~ ) = mult_big_int;;
let ( /~ ) = div_big_int;;
let ( %~ ) = mod_big_int;;
let ( =~ ) = eq_big_int;;
let ( <>~ ) x y = not (eq_big_int x y);;
let ( <~ ) x y = lt_big_int x y;;
let ( <=~ ) x y = le_big_int x y;;
let ( >~ ) x y = gt_big_int x y;;
let ( >=~ ) x y = ge_big_int x y;;

let zero = zero_big_int;;
let one = unit_big_int;;

let build_env funcs = let tbl = Hashtbl.create 10 in
  List.iter (fun func -> Hashtbl.add tbl func.name func) funcs;
  {function_table = tbl}

let env_lookup env name = Hashtbl.find env.function_table name

let rec comp env func args = 
  let vars = Hashtbl.create 10 in
  (List.iter2 (fun name arg -> Hashtbl.add vars name arg) func.params args;
   match eval_stmt env vars func.body with Some x -> x | None -> zero)

and eval_stmt env vars = function
  Expr expr -> ignore (eval_expr env vars expr); None
| If (expr, t, e) ->
    if (eval_expr env vars expr) <>~ zero then eval_stmt env vars t else eval_stmt env vars e
| While (expr, stmt) ->
    let res = ref None in
    while (eval_expr env vars expr) <>~ zero && (res := eval_stmt env vars stmt; !res = None) do () done;
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
    print_string (string_of_big_int (eval_expr env vars (List.hd args))); print_newline (); zero
| Call (name, args) ->
    let func = Hashtbl.find env.function_table name in
    comp env func (List.map (eval_expr env vars) args)   
| UnalyPlus expr -> eval_expr env vars expr
| UnalyMinus expr -> zero -~ (eval_expr env vars expr)
| Mul (lhs, rhs) -> (eval_expr env vars lhs) *~ (eval_expr env vars rhs)
| Div (lhs, rhs) -> (eval_expr env vars lhs) /~ (eval_expr env vars rhs)
| Mod (lhs, rhs) -> (eval_expr env vars lhs) %~ (eval_expr env vars rhs)
| Add (lhs, rhs) -> (eval_expr env vars lhs) +~ (eval_expr env vars rhs)
| Sub (lhs, rhs) -> (eval_expr env vars lhs) -~ (eval_expr env vars rhs)
| Lt (lhs, rhs) -> if (eval_expr env vars lhs) <~ (eval_expr env vars rhs) then one else zero
| Gt (lhs, rhs) -> if (eval_expr env vars lhs) >~ (eval_expr env vars rhs) then one else zero
| Lteq (lhs, rhs) -> if (eval_expr env vars lhs) <=~ (eval_expr env vars rhs) then one else zero
| Gteq (lhs, rhs) -> if (eval_expr env vars lhs) >=~ (eval_expr env vars rhs) then one else zero
| Eq (lhs, rhs) -> if (eval_expr env vars lhs) =~ (eval_expr env vars rhs) then one else zero
| Ne (lhs, rhs) -> if (eval_expr env vars lhs) <>~ (eval_expr env vars rhs) then one else zero
| Assign (name, expr) -> (let v = (eval_expr env vars expr) in
    Hashtbl.add vars name v; v)
