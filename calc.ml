let src = "\
int print(int);

int isprime(int n) {
    int i = 2;
    if (n < 2) return 0;
    while (i < n) {
        if (n % i == 0) return 0;
        i = i + 1;
    }
    return 1;
}

int main() {
    int i = 1;
    while (i <= 100) {
        if (isprime(i)) {
            print(i);
        }
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
