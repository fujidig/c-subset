(* File calc.ml *)
let lexbuf = Lexing.from_channel stdin in
    let result = Parser.main Lexer.token lexbuf in
        print_endline (String.concat "\n" result)