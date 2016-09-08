{
open Parser
open Big_int
}
rule token = parse
    [' ' '\t' '\r' '\n']     { token lexbuf }
    | "int"             { INT }
    | "if"              { IF }
    | "else"            { ELSE }
    | "while"           { WHILE }
    | "goto"            { GOTO }
    | "return"          { RETURN }
    | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_']*
                        { IDENTIFIER (Lexing.lexeme lexbuf) }
    | ['0'-'9']+        { CONSTANT (big_int_of_string (Lexing.lexeme lexbuf)) }
    | "("               { RPAREN }
    | ")"               { LPAREN }
    | ","               { COMMA }
    | "+"               { PLUS }
    | "-"               { MINUS }
    | "*"               { ASTERISK }
    | "/"               { SLASH }
    | "%"               { PERCENT }
    | "="               { EQUAL }
    | ";"               { SEMICOLON }
    | ":"               { COLON }
    | "{"               { RBRACE }
    | "}"               { LBRACE }
    | "<"               { LT }
    | ">"               { GT }
    | "<="              { LE_OP }
    | ">="              { GE_OP }
    | "=="              { EQ_OP }
    | "!="              { NE_OP }
    | eof               { EOF }
