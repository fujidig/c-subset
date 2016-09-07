(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }
    | "int"             { INT }
    | "if"              { IF }
    | "else"            { ELSE }
    | "while"           { WHILE }
    | "goto"            { GOTO }
    | "return"          { RETURN }
    | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_']*
                        { IDENTIFIER (Lexing.lexeme lexbuf) }
    | ['0'-'9']+        { CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
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
    | "<="              { LE_OP }
    | ">="              { GE_OP }
    | "=="              { EQ_OP }
    | "!="              { NE_OP }
    | eof               { EOF }
