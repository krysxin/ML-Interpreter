%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR  
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ ANDLET 
%token RARROW FUN 
%token DFUN 
%token REC 

%token QUIT

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  //| LET x=ID EQ e=Expr SEMISEMI { Decl (x,e)} 
  | LET e_ls=AndLetExpr SEMISEMI {Decl e_ls} 
  | LET REC x1=ID EQ FUN x2=ID RARROW e=Expr SEMISEMI { RecDecl (x1, x2, e)}
  | QUIT SEMISEMI {QuitDecl}

Expr :
    e=IfExpr { e }
  | e=ORExpr { e }  
  | e=LetExpr { e } 
  | e=FunExpr { e } 
  | e=DFunExpr { e } 
  | e=LetRecExpr { e } 
    //| e=LTExpr { e }


LetExpr :
  LET e_ls=AndLetExpr IN e2=Expr { LetExp (e_ls, e2) } 
  

AndLetExpr :
   x=ID EQ e=Expr { [(x,e)] }
  | x=ID EQ e1=Expr ANDLET e2=AndLetExpr { (x,e1) :: e2 } 
  | x=ID args=MultiArgs EQ e=Expr { [(x, argstoFun args e )] } 
  | x=ID args=MultiArgs EQ e=Expr ANDLET e2=AndLetExpr { (x, argstoFun args e ) :: e2 } 
  


MultiArgs :
  x=ID { [x] }
| x=ID e=MultiArgs { x :: e }

FunExpr : 
   FUN e=FunMultiAgrsExpr { e } 
  

DFunExpr :
   DFUN x=ID RARROW e=Expr { DFunExp (x,e)} 

FunMultiAgrsExpr : 
   x=ID RARROW e=Expr { FunExp (x,e) }
  | x=ID e=FunMultiAgrsExpr { FunExp (x,e) }

LetRecExpr :
   LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x1,x2,e1,e2)} 

ORExpr :  
 l=ANDExpr OR r=ANDExpr { BinOp (Or, l, r) }
| e=ANDExpr { e }

ANDExpr :  
 l=LTExpr AND r=LTExpr { BinOp (And, l, r) }
| e=LTExpr { e }


LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) } 
  | e=AppExpr { e } 
  
AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }
  

InfixExpr :
    LPAREN PLUS RPAREN { FunExp ("x", FunExp ("y", BinOp (Plus, Var "x", Var "y")))}
  | LPAREN MULT RPAREN { FunExp ("x", FunExp ("y", BinOp (Mult, Var "x", Var "y")))}
  | LPAREN LT RPAREN { FunExp ("x", FunExp ("y", BinOp (Lt, Var "x", Var "y")))}
  | LPAREN OR RPAREN { FunExp ("x", FunExp ("y", BinOp (Or, Var "x", Var "y"))) }
  | LPAREN AND RPAREN { FunExp ("x", FunExp ("y", BinOp (And, Var "x", Var "y"))) }
  
    
AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
  | e=InfixExpr { e }

// AExprListExpr : 
//    e=AExpr rest=AExprListExpr { e :: rest }
//   | e=AExpr { [e] }
  

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
