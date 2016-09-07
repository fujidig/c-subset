/* https://www.lysator.liu.se/c/ANSI-C-grammar-y.html */

%{
open Ast
%}

%token RPAREN LPAREN COMMA PLUS MINUS ASTERISK SLASH PERCENT
%token EQUAL SEMICOLON COLON RBRACE LBRACE
%token <string> IDENTIFIER
%token <int> CONSTANT
%token LE_OP GE_OP EQ_OP NE_OP
%token INT
%token IF ELSE WHILE GOTO RETURN
%token EOF

%start main

%type <Ast.function_definition list> main

%%

primary_expression
    : IDENTIFIER { Identifier $1 }
    | CONSTANT { Constant $1 }
    | RPAREN expression LPAREN { $2 }
    ;

postfix_expression
    : primary_expression { $1 }
    | IDENTIFIER RPAREN LPAREN { Call ($1, []) }
    | IDENTIFIER RPAREN argument_expression_list LPAREN { Call ($1, $3) }
    ;

argument_expression_list
    : assignment_expression { [$1] }
    | argument_expression_list COMMA assignment_expression { $1 @ [$3] }
    ;

unary_expression
    : postfix_expression { $1 }
    | PLUS unary_expression { UnalyPlus $2 }
    | MINUS unary_expression { UnalyMinus $2 }
    ;

multiplicative_expression
    : unary_expression { $1 }
    | multiplicative_expression ASTERISK unary_expression { Mul ($1, $3) }
    | multiplicative_expression SLASH unary_expression { Div ($1, $3) }
    | multiplicative_expression PERCENT unary_expression { Mod ($1, $3) }
    ;

additive_expression
    : multiplicative_expression { $1 }
    | additive_expression PLUS multiplicative_expression { Add ($1, $3) }
    | additive_expression MINUS multiplicative_expression { Sub ($1, $3) }
    ;

relational_expression
    : additive_expression { $1 }
    | relational_expression '<' additive_expression { Lt ($1, $3) }
    | relational_expression '>' additive_expression { Gt ($1, $3) }
    | relational_expression LE_OP additive_expression { Lteq ($1, $3) }
    | relational_expression GE_OP additive_expression { Gteq ($1, $3) }
    ;

equality_expression
    : relational_expression { $1 }
    | equality_expression EQ_OP relational_expression { Eq ($1, $3) }
    | equality_expression NE_OP relational_expression { Ne ($1, $3) }
    ;

assignment_expression
    : equality_expression { $1 }
    | IDENTIFIER EQUAL assignment_expression { Assign ($1, $3) }
    ;

expression
    : assignment_expression { $1 }
    ;

declaration
    : type_specifier SEMICOLON { [] }
    | type_specifier declarator_list SEMICOLON { $2 }
    ;

declarator_list
    : declarator { [($1, Constant 0)] }
    | declarator_list COMMA declarator { $1 @ [($3, Constant 0)] }
    ;

type_specifier
    : INT { }
    ;

declarator
    : IDENTIFIER { $1 }
    ;

function_declarator
    : type_specifier declarator RPAREN LPAREN { ($2, []) }
    | type_specifier declarator RPAREN parameter_type_list LPAREN { ($2, $4) }
    | type_specifier declarator RPAREN identifier_list LPAREN { ($2, $4) }
    ;

function_declaration
    : function_declarator SEMICOLON { }
    ;

parameter_type_list
    : parameter_list { $1 }
    ;

parameter_list
    : parameter_declaration { [$1] }
    | parameter_list COMMA parameter_declaration { $1 @ [$3] }
    ;

parameter_declaration
    : type_specifier declarator { $2 }
    | type_specifier { "" }
    ;

identifier_list
    : IDENTIFIER { [$1] }
    | identifier_list COMMA IDENTIFIER { $1 @ [$3] }
    ;

statement
    : compound_statement { $1 }
    | expression_statement { $1 }
    | selection_statement { $1 }
    | iteration_statement { $1 }
    | jump_statement { $1 }
    ;

compound_statement
    : RBRACE LBRACE { Block ([], []) }
    | RBRACE statement_list LBRACE { Block ([], $2) }
    | RBRACE declaration_list LBRACE { Block ($2, []) }
    | RBRACE declaration_list statement_list LBRACE { Block ($2, $3) }
    ;

declaration_list
    : declaration { $1 }
    | declaration_list declaration { $1 @ $2 }
    ;

statement_list
    : statement { [$1] }
    | statement_list statement { $1 @ [$2] }
    ;

expression_statement
    : SEMICOLON { Block ([], []) }
    | expression SEMICOLON { Expr $1 }
    ;

selection_statement
    : IF RPAREN expression LPAREN statement { If ($3, $5, (Block ([], []))) }
    | IF RPAREN expression LPAREN statement ELSE statement { If ($3, $5, $7) }
    ;

iteration_statement
    : WHILE RPAREN expression LPAREN statement { While ($3, $5) }
    ;

jump_statement
    : RETURN SEMICOLON { Return (Constant 0) }
    | RETURN expression SEMICOLON { Return $2 }
    ;

external_declaration
    : function_definition { Some $1 }
    | function_declaration { None }
    ;

function_definition
    : function_declarator compound_statement {
      match $1 with (name, params) -> {
        name = name;
        params = params;
        body = $2
      }
    }
    ;

translation_unit
    : { [] }
    | translation_unit external_declaration {
      match $2 with Some def -> $1 @ [def] | None -> $1
    }
    ;

main : translation_unit EOF { $1 }
     ;