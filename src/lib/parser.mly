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

%right     THEN ELSE DO IN
%nonassoc  ASSIGN
%left      OR
%left      AND
%nonassoc  EQ NE GT GE LT LE
%left      PLUS MINUS
%left      TIMES DIV MOD
%right     POW
%nonassoc  UMINUS

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
 | IF t=exp THEN x=exp y=option(ELSE y=exp {y})               {$loc, IfExp (t, x, y)}
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
 | ds=decs_beginning_with_variable                            {ds}
 | ds=decs_beginning_with_function                            {ds}
 | ds=decs_beginning_with_type                                {ds}

decs_beginning_with_variable:
 | d=vardec                                                   {d :: []}
 | d=vardec ds=decs_beginning_with_variable                   {d :: ds}
 | d=vardec ds=decs_beginning_with_function                   {d :: ds}
 | d=vardec ds=decs_beginning_with_type                       {d :: ds}

decs_beginning_with_function:
 | d=mutualfundecs                                            {d :: []}
 | d=mutualfundecs ds=decs_beginning_with_variable            {d :: ds}
 | d=mutualfundecs ds=decs_beginning_with_type                {d :: ds}

decs_beginning_with_type:
 | d=mutualtypedecs                                           {d :: []}
 | d=mutualtypedecs ds=decs_beginning_with_variable           {d :: ds}
 | d=mutualtypedecs ds=decs_beginning_with_function           {d :: ds}

vardec:
 | VAR v=ID t=type_constraint "=" e=exp                       {$loc, VarDec (v, t, e)}

fundec:
 | FUNCTION f=ID "(" p=params ")" t=type_constraint "=" b=exp {$loc, (f, p, t, b)}

typedec:
 | TYPE t=ID "=" ty=type_constructor                          {$loc, (t, ty)}

mutualfundecs:
 | ds=nonempty_list(fundec)                                   {$loc, MutualFunctionDecs ds}

mutualtypedecs:
 | ds=nonempty_list(typedec)                                  {$loc, MutualTypeDecs ds}

type_constraint:
 | c=option(":" t=ID {$loc(t), t})                            {c}

type_constructor:
 | ty=ID                                                      {$loc, NameCons ty}

params:
 | f=separated_list(",", param)                               {f}

param:
 | n=ID ":" t=ID                                              {$loc, (n, t)}
