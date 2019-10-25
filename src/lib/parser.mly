// parser.mly

%{
  open Absyn
%}

%token <bool>          LOGIC
%token <int>           INTEGER
%token <string>        STRING
%token <float>         REAL
%token <Symbol.symbol> ID
%token                 IF THEN ELSE
%token                 WHILE DO BREAK
%token                 LET IN END
%token                 VAR
%token                 LPAREN "(" RPAREN ")"
%token                 COLON ":" COMMA "," SEMI ";"
%token                 PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%" POW "^"
%token                 EQ "=" NE "<>"
%token                 LT "<" LE "<=" GT ">" GE ">="
%token                 AND "&" OR "|"
%token                 ASSIGN ":="
%token                 EOF

%start <Absyn.lexp> program

%%

program:
 | x=exp EOF {x}

exp:
 | x=LOGIC              {$loc, BoolExp x}
 | x=INTEGER            {$loc, IntExp x}
 | WHILE t=exp DO b=exp {$loc, WhileExp (t, b)}
 | LET d=decs IN e=exp  {$loc, LetExp (d, e)}
 | v=var                {$loc, VarExp v}

decs:
 | l=list(dec) {l}

dec:
 | v=vardec {v}

vardec:
 | VAR v=ID ":" t=ID ":=" e=exp {$loc, VarDec (v, Some ($loc(t), t), e)}
 | VAR v=ID ":=" e=exp {$loc, VarDec (v, None, e)}

var:
 | x=ID {$loc, SimpleVar x}


                      (*

let var idade : int := 2 + 3
    var peso := idade * 5 + 1
in
    printint(idade - peso)



                       *)
