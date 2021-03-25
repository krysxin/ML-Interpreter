%{
open TySyntax
%}

%token INT BOOL
%token LIST
%token LPAREN RPAREN EOF
%token ARROW
%token <string> TYVAR

%start toplevel
%type <TySyntax.ty> toplevel
%%

toplevel :
    t=TFun EOF { t }

TFun :
    t=TLit { t }
  | t1=TLit ARROW t2=TFun { TyFun (t1, t2) }

TLit :
    INT { TyInt }
  | BOOL { TyBool }
  | name=TYVAR { TyVar name }
  | LPAREN t=TFun RPAREN { t }
  | t=TLit LIST { TyList t }
