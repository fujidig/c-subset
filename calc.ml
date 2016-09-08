let src = "\
int print(int);

int main() {
    int i = 1, prod = 1;
    while (i <= 30) {
        print(prod);
        prod = prod * i;
        i = i + 1;
    }
}
";;

print_string src

let ast = 
  let lexbuf = Lexing.from_string src in Parser.main Lexer.token lexbuf;;

let env = Eval.build_env ast;;

(* print_int (Eval.comp env (Eval.env_lookup env "main") []); *)
ignore (Eval.comp env (Eval.env_lookup env "main") []);

print_newline ();
