{
  open ExprParser

  exception Eof
  let lnum = ref 1
  let cnum = ref 0
  let get_len lexbuf = String.length (Lexing.lexeme lexbuf)
  let upd_cnum lexbuf = cnum := !cnum + get_len lexbuf
  let reset_cnum () = cnum := 0
}

let letter = ['a'-'z' 'A'-'Z' '_']
let number = ['0' - '9']
let identity = letter (letter | number)*

rule token = parse
    [' ' '\t']                   { upd_cnum lexbuf; token lexbuf }
  | "!="                         { upd_cnum lexbuf; NE }
  | "=="                         { upd_cnum lexbuf; EQ }
  | '+'                          { upd_cnum lexbuf; ADD }
  | '-'                          { upd_cnum lexbuf; SUB }
  | '*'                          { upd_cnum lexbuf; MUL }
  | '/'                          { upd_cnum lexbuf; DIV }
  | '%'                          { upd_cnum lexbuf; MOD }
  | '!'                          { upd_cnum lexbuf; NOT }
  | "&&"                         { upd_cnum lexbuf; LAND }
  | "||"                         { upd_cnum lexbuf; LOR }
  | "<<"                         { upd_cnum lexbuf; SHIFTL }
  | ">>"                         { upd_cnum lexbuf; SHIFTR }
  | "<="                         { upd_cnum lexbuf; LE }
  | ">="                         { upd_cnum lexbuf; GE }
  | '<'                          { upd_cnum lexbuf; LT }
  | '>'                          { upd_cnum lexbuf; GT }
  | '('                          { upd_cnum lexbuf; LPAREN }
  | ')'                          { upd_cnum lexbuf; RPAREN }
  | '['                          { upd_cnum lexbuf; LSQUARE }
  | ']'                          { upd_cnum lexbuf; RSQUARE }
  | number+ as num               { upd_cnum lexbuf; NUM (int_of_string num) }
  | identity as id               { upd_cnum lexbuf; VAR id }
  | eof                          { EOF }
