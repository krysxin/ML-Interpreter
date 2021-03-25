rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "'"['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9'''''_']*
    {
      let str = Lexing.lexeme lexbuf in
      TyParser.TYVAR (String.sub str 1 (String.length str - 1))
    }

| "list" { TyParser.LIST }
| "int" { TyParser.INT }
| "bool" { TyParser.BOOL }
| "(" { TyParser.LPAREN }
| ")" { TyParser.RPAREN }
| "->" { TyParser.ARROW }

| eof { TyParser.EOF }
