(* File calc.ml *)
let lexbuf = Lexing.from_channel stdin in
    Parser.main Lexer.token lexbuf