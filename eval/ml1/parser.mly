%{
open Syntax
%}

%token <int> INT

%token LPAREN RPAREN
%token PLUS MINUS TIMES LT
%token IF THEN ELSE
%token TRUE FALSE
%token EOF

%nonassoc ELSE
%left LT
%left PLUS MINUS
%left TIMES
%nonassoc UNARY

%start main
%type <Syntax.exp> main

%%

main:
    exp EOF { $1 }

exp:
    INT { IntLit $1 }
  | TRUE { BoolLit true }
  | FALSE { BoolLit false }
  | LPAREN exp RPAREN { $2 }
  | MINUS exp %prec UNARY { Op (Minus, IntLit 0, $2) }
  | exp PLUS exp { Op (Plus, $1, $3) }
  | exp MINUS exp { Op (Minus, $1, $3) }
  | exp TIMES exp { Op (Times, $1, $3) }
  | exp LT exp { Op (Lt, $1, $3) }
  | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
