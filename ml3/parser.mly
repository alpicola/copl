%{
open Syntax
%}

%token <int> INT
%token <string> VAR

%token LPAREN RPAREN
%token PLUS MINUS TIMES LT
%token IF THEN ELSE
%token LET REC EQ IN
%token FUN ARROW
%token TRUE FALSE
%token EOF

%nonassoc ELSE IN ARROW
%left LT
%left PLUS MINUS
%left TIMES
%nonassoc UNARY
%left VAR INT TRUE FALSE LPAREN

%start main
%type <Syntax.exp> main

%%

main:
    exp EOF { $1 }

arg:
    INT { IntLit $1 }
  | TRUE { BoolLit true }
  | FALSE { BoolLit false }
  | VAR { Var $1 }
  | LPAREN exp RPAREN { $2 }

exp:
    arg { $1 }
  | exp arg { App ($1, $2) }
  | MINUS exp %prec UNARY { Op (Minus, IntLit 0, $2) }
  | exp PLUS exp { Op (Plus, $1, $3) }
  | exp MINUS exp { Op (Minus, $1, $3) }
  | exp TIMES exp { Op (Times, $1, $3) }
  | exp LT exp { Op (Lt, $1, $3) }
  | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
  | LET REC VAR EQ FUN VAR ARROW exp IN exp { LetRec ($3, $6, $8, $10) }
  | LET VAR EQ exp IN exp { Let ($2, $4, $6) }
  | FUN VAR ARROW exp { Fun ($2, $4) }
