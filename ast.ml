type expr =
  Constant of Big_int.big_int
| Identifier of string
| Call of string * expr list
| UnalyPlus of expr
| UnalyMinus of expr
| Mul of expr * expr
| Div of expr * expr
| Mod of expr * expr
| Add of expr * expr
| Sub of expr * expr
| Lt of expr * expr
| Gt of expr * expr
| Lteq of expr * expr
| Gteq of expr * expr
| Eq of expr * expr
| Ne of expr * expr
| Assign of string * expr

type definedVar = string * expr

type stmt =
  Expr of expr
| If of expr * stmt * stmt
| While of expr * stmt
| Block of definedVar list * stmt list
| Return of expr

type function_definition = {
    name : string;
    params : string list;
    body: stmt
}