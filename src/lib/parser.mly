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
%token                 VAR FUNCTION TYPE
%token                 LPAREN "(" RPAREN ")"
%token                 COLON ":" COMMA "," SEMI ";"
%token                 PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%" POW "^" UMINUS
%token                 EQ "=" NE "<>"
%token                 LT "<" LE "<=" GT ">" GE ">="
%token                 AND "&" OR "|"
%token                 ASSIGN ":="
%token                 EOF

%nonassoc  ASSIGN
%left      OR
%left      AND
%nonassoc  EQ NE GT GE LT LE
%left      PLUS MINUS
%left      TIMES DIV MOD
%right     POW
%nonassoc  UMINUS
%nonassoc  DO IN

%start <Absyn.lexp> program

%%

program:
 | x=exp EOF                                                  {x}

exp:
 | x=LOGIC                                                    {$loc, BoolExp x}
 | x=INTEGER                                                  {$loc, IntExp x}
 | x=REAL                                                     {$loc, RealExp x}
 | x=STRING                                                   {$loc, StringExp x}
 | x=exp o=binop y=exp                                        {$loc, OpExp (o, x, y)}
 | o="-" x=exp %prec UMINUS                                   {$loc, OpExp (MinusOp, ($loc(o), IntExp 0), x)}
 | v=var                                                      {$loc, VarExp v}
 | v=var ":=" e=exp                                           {$loc, AssignExp (v, e)}
 | WHILE t=exp DO b=exp                                       {$loc, WhileExp (t, b)}
 | BREAK                                                      {$loc, BreakExp}
 | LET d=decs IN e=exp                                        {$loc, LetExp (d, e)}
 | IF t=exp THEN x=exp ELSE y=option(exp)                     {$loc, IfExp (t, x, y)}
 | f=ID "(" a=args ")"                                        {$loc, CallExp (f, a)}
 | "(" es=exps ")"                                            {$loc, SeqExp es}

%inline binop:
 | "+"                                                        {PlusOp}
 | "-"                                                        {MinusOp}
 | "*"                                                        {TimesOp}
 | "/"                                                        {DivOp}
 | "%"                                                        {ModOp}
 | "^"                                                        {PowOp}
 | "="                                                        {EqOp}
 | "<>"                                                       {NeOp}
 | "<"                                                        {LtOp}
 | "<="                                                       {LeOp}
 | ">"                                                        {GtOp}
 | ">="                                                       {GeOp}
 | "&"                                                        {AndOp}
 | "|"                                                        {OrOp}

exps:
 | es=separated_list(";", exp)                                {es}


args:
 | es=separated_list(",", exp)                                {es}

var:
 | x=ID                                                       {$loc, SimpleVar x}

decs:
 | ds=list(dec)                                               {ds}

dec:
 | d=vardec                                                   {$loc, d}
 | d=mutualtypedecs                                           {$loc, d}
 | d=mutualfundecs                                            {$loc, d}

vardec:
 | VAR v=ID t=type_constraint ":=" e=exp                      {VarDec (v, t, e)}

fundec:
 | FUNCTION f=ID "(" p=params ")" t=type_constraint "=" b=exp {$loc, (f, p, t, b)}

typedec:
 | TYPE t=ID "=" ty=type_constructor                          {$loc, (t, ty)}

mutualfundecs:
 | ds=nonempty_list(fundec)                                   {MutualFunctionDecs ds}

mutualtypedecs:
 | ds=nonempty_list(typedec)                                  {MutualTypeDecs ds}

type_constraint:
 | c=option(":" t=ID {$loc(t), t})                            {c}

type_constructor:
 | ty=ID                                                      {$loc, NameCons ty}

params:
 | f=separated_list(",", param)                               {f}

param:
 | n=ID "=" t=ID                                              {$loc, (n, t)}
