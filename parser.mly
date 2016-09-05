/* https://www.lysator.liu.se/c/ANSI-C-grammar-y.html */

%token RPAREN LPAREN COMMA PLUS MINUS ASTERISK SLASH PERCENT
%token EQUAL SEMICOLON COLON RBRACE LBRACE
%token <string> IDENTIFIER
%token CONSTANT
%token LE_OP GE_OP EQ_OP NE_OP
%token INT
%token IF ELSE WHILE GOTO RETURN
%token EOF

%start main

%type <string list> main

%%

primary_expression
    : IDENTIFIER { }
    | CONSTANT { }
    | RPAREN expression LPAREN { }
    ;

postfix_expression
    : primary_expression { }
    | postfix_expression RPAREN LPAREN { }
    | postfix_expression RPAREN argument_expression_list LPAREN { }
    ;

argument_expression_list
    : assignment_expression { }
    | argument_expression_list COMMA assignment_expression { }
    ;

unary_expression
    : postfix_expression { }
    | PLUS unary_expression { }
    | MINUS unary_expression { }
    ;

multiplicative_expression
    : unary_expression { }
    | multiplicative_expression ASTERISK unary_expression { }
    | multiplicative_expression SLASH unary_expression { }
    | multiplicative_expression PERCENT unary_expression { }
    ;

additive_expression
    : multiplicative_expression { }
    | additive_expression PLUS multiplicative_expression { }
    | additive_expression MINUS multiplicative_expression { }
    ;

relational_expression
    : additive_expression { }
    | relational_expression '<' additive_expression { }
    | relational_expression '>' additive_expression { }
    | relational_expression LE_OP additive_expression { }
    | relational_expression GE_OP additive_expression { }
    ;

equality_expression
    : relational_expression { }
    | equality_expression EQ_OP relational_expression { }
    | equality_expression NE_OP relational_expression { }
    ;

assignment_expression
    : equality_expression { }
    | unary_expression EQUAL assignment_expression { }
    ;

expression
    : assignment_expression { }
    | expression COMMA assignment_expression { }
    ;

declaration
    : type_specifier SEMICOLON { }
    | type_specifier declarator_list SEMICOLON { }
    ;

declarator_list
    : declarator { }
    | declarator_list COMMA declarator { }
    ;

type_specifier
    : INT { }
    ;

declarator
    : IDENTIFIER { $1 }
    ;

function_declarator
    : type_specifier declarator RPAREN LPAREN { $2 }
    | type_specifier declarator RPAREN parameter_type_list LPAREN { $2 }
    | type_specifier declarator RPAREN identifier_list LPAREN { $2 }
    ;

function_declaration
    : function_declarator SEMICOLON { $1 }
    ;

parameter_type_list
    : parameter_list { }
    ;

parameter_list
    : parameter_declaration { }
    | parameter_list COMMA parameter_declaration { }
    ;

parameter_declaration
    : type_specifier declarator { }
    | type_specifier { }
    ;

identifier_list
    : IDENTIFIER { }
    | identifier_list COMMA IDENTIFIER { }
    ;

statement
    : labeled_statement { }
    | compound_statement { }
    | expression_statement { }
    | selection_statement { }
    | iteration_statement { }
    | jump_statement { }
    ;

labeled_statement
    : IDENTIFIER COLON statement { }
    ;

compound_statement
    : RBRACE LBRACE { }
    | RBRACE statement_list LBRACE { }
    | RBRACE declaration_list LBRACE { }
    | RBRACE declaration_list statement_list LBRACE { }
    ;

declaration_list
    : declaration { }
    | declaration_list declaration { }
    ;

statement_list
    : statement { }
    | statement_list statement { }
    ;

expression_statement
    : SEMICOLON { }
    | expression SEMICOLON { }
    ;

selection_statement
    : IF RPAREN expression LPAREN statement { }
    | IF RPAREN expression LPAREN statement ELSE statement { }
    ;

iteration_statement
    : WHILE RPAREN expression LPAREN statement { }
    ;

jump_statement
    : GOTO IDENTIFIER SEMICOLON { }
    | RETURN SEMICOLON { }
    | RETURN expression SEMICOLON { }
    ;

external_declaration
    : function_definition { "" }
    | function_declaration { $1 }
    ;

function_definition
    : function_declarator compound_statement { }
    ;

translation_unit
    : { [] }
    | external_declaration { [$1] }
    | translation_unit external_declaration { $1 @ [$2] }
    ;

main : translation_unit EOF { $1 }
     ;